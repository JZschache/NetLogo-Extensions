package de.qlearning

import java.util.{ List => JList }
import java.io.File

import scala.collection.JavaConverters._

import akka.actor.ActorSystem
import akka.actor.Props
import akka.agent.{ Agent => AkkaAgent }
import akka.dispatch.Await
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._

import org.nlogo.api._
import org.nlogo.api.Syntax._
import org.nlogo.api.ScalaConversions._

import com.typesafe.config.ConfigFactory

import de.util.PerformanceMeasure


object QLSystem {
  
  // name of NetLogo extension
  val EXTENSION_NAME = "ql"
  // path of configuration file
  val confFile = "conf/application.conf"
    
  // starting the ActorSystem
  implicit val system = ActorSystem("QLSystem", ConfigFactory.parseFile(new File(confFile)))
  
  val config = system.settings.config
  val cfgstr = "netlogo"
  // the timeout is used to wait for NetLogo, especially: GetNLGroupChoices from HeadlessEnvironments, but also: init, clearAll 
  val defaultWaitDuration = config.getInt(cfgstr + ".timeout_ms") milliseconds
  implicit val timeout = Timeout(defaultWaitDuration)
  
  // a number of headless instances of NetLogo are loaded in the background
  // they are used to simultaneousely execute reward procedures
  // a router is used to distribute jobs to the headless instances
  val conHeadlessEnv = config.getInt(cfgstr + ".concurrent-headless-environments")
  val netLogoRouter = system.actorOf(Props().withRouter(NetLogoHeadlessRouter(conHeadlessEnv)))
  
  // three AkkaAgents measure performance of the program
  val betweenTickPerf = AkkaAgent(PerformanceMeasure())
  val handleGroupPerf = AkkaAgent(PerformanceMeasure())
  val guiInterPerf = AkkaAgent(PerformanceMeasure())
  
  val headlessIdlePerf = AkkaAgent(Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure()))
  val headlessHandleNLGroupPerf = AkkaAgent(Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure()))
  val headlessHandleNLGroupChoicePerf = AkkaAgent(Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure()))
  
  val headlessAnswerNLPerf = AkkaAgent(PerformanceMeasure())
//  
//  val mailboxNLGroupsList = AkkaAgent(0)
//  val mailboxNLGroupChoicesList = AkkaAgent(0)
//  val mailboxGetNLGroupChoices = AkkaAgent(0)
//  
//  val maxQueueLength = AkkaAgent(0)
  
  // the supervisor controls the simulation
  val netLogoSuper = system.actorOf(Props(new NetLogoSupervisor(netLogoRouter)))//.withDispatcher("pinned-dispatcher"))
      
  // this map connects an NetLogo-agent (turtle / patch) to a QLAgent that performs the learning 
  // it therefore also holds all data about the agents
  val qlDataMap = AkkaAgent(Map[Agent, AkkaAgent[QLAgent]]())

}

/**
 * the extension class needed by NetLogo
 */
class QLExtension extends DefaultClassManager {
  import QLSystem._
  
  override def load(manager: PrimitiveManager) {
    // observer primitives
    manager.addPrimitive("init", new Init)
    manager.addPrimitive("create-group", new CreateGroup)
    manager.addPrimitive("set-group-structure", new NewGroupStructure)
    manager.addPrimitive("start", new Start)
    manager.addPrimitive("stop", new Stop)
    manager.addPrimitive("decay-exploration", new DecreaseExperimenting)
    
    manager.addPrimitive("get-group-list", new GetGroupList)
    manager.addPrimitive("get-agents", new GetAgents)
    manager.addPrimitive("get-decisions", new GetDecisions)
    manager.addPrimitive("set-rewards", new SetRewards)
    
    manager.addPrimitive("get-performance", new GetPerformance)
  }
    
  
  override def additionalJars: JList[String] = {
    val list : java.util.List[String] =  new java.util.ArrayList[String]
    list.add("akka-actor-2.0.5.jar")
    list.add("akka-agent-2.0.5.jar")
    list.add("config-1.0.2.jar")
    list.add("colt-1.2.0.jar")
    list.add("scala-stm_2.9.1-0.5.jar")
    list
  }
  
  override def clearAll() {
    // stop all data agents and reset the map
    // this may take a while
    qlDataMap send {map => {
      map.values.foreach(_.close)
      Map[org.nlogo.api.Agent, AkkaAgent[QLAgent]]()
    }}
    // wait till complete
    qlDataMap.await
  }
    
}

//////////////////////////////////////////////
/// implementations of new NetLogo objects /// 
//////////////////////////////////////////////

trait hasNLAgents { val nlAgents: List[org.nlogo.api.Agent] }

/**
 * a new NetLogo object that is used to represent the group structure 
 * it also specifies the alternatives available to each agent
 */
case class NLGroup(nlAgents: List[org.nlogo.api.Agent], 
                   qlAgents: List[AkkaAgent[QLAgent]], 
                   alternatives: List[List[String]]) extends org.nlogo.api.ExtensionObject with hasNLAgents {
  require(nlAgents.size == alternatives.size)
  
  def dump(readable: Boolean, exporting: Boolean, reference: Boolean): String = toString
  def getExtensionName: String = QLSystem.EXTENSION_NAME
  def getNLTypeName: String = "NLGroup"
  def recursivelyEqual(obj: AnyRef): Boolean = equals(obj)
}

/**
 * a new NetLogo object that is used to share data between Akka and NetLogo
 */
case class NLGroupChoice(nlAgents: List[org.nlogo.api.Agent], 
                         qlAgents: List[AkkaAgent[QLAgent]], 
                         choices: List[String], 
                         rewards: List[Double]) extends org.nlogo.api.ExtensionObject with hasNLAgents {
  require(nlAgents.size == qlAgents.size && qlAgents.size == choices.size)
  
  def dump(readable: Boolean, exporting: Boolean, reference: Boolean): String = toString
  def getExtensionName: String = QLSystem.EXTENSION_NAME
  def getNLTypeName: String = "NLGroupChoice"
  def recursivelyEqual(obj: AnyRef): Boolean = equals(obj)
}

/////////////////////////////////////////////////
/// implementations of reporters and commands /// 
/////////////////////////////////////////////////

/**
 * the communication of parameters (list of NLGroupChoices) that are set 
 * by an NetLogoHeadlessActor and used from an NetLogoHeadlessApp in reward function calls
 * 
 * needs the id of the NetLogoHeadlessActor and returns an object of type List[NLGroupChoice] 
 */
class GetGroupList extends DefaultReporter {
  import QLSystem._
  import NetLogoHeadlessActor.{GetNLGroupChoices, NLGroupChoicesList}
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](NumberType), ListType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    
    headlessAnswerNLPerf send { _.start(scala.compat.Platform.currentTime)}
    
    val future = (netLogoRouter ? GetNLGroupChoices(args(0).getIntValue)).mapTo[NLGroupChoicesList]
    // we have to block because NetLogo is waiting for a result
    val result = try {
      Await.result(future, defaultWaitDuration).list
    } catch {
      case e: java.util.concurrent.TimeoutException =>
        
        println("Timeout when waiting for HeadlessEnvironment with id: " + args(0).getIntValue)
        Nil
    }
    
    headlessAnswerNLPerf send { _.end(scala.compat.Platform.currentTime) }
      
    result.toLogoList
  }
}

/**
 * takes an NLGroupChoice and returns its agents 
 */
class GetAgents extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType), ListType)
  
  def report(args: Array[Argument], c: Context): AnyRef =
    args(0).get.asInstanceOf[hasNLAgents].nlAgents.toLogoList
}

/**
 * takes an NLGroupChoice and returns its decisions 
 */
class GetDecisions extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType), ListType)
  
  def report(args: Array[Argument], c: Context): AnyRef = 
    args(0).get.asInstanceOf[NLGroupChoice].choices.toLogoList
}

/**
 * takes an NLGroupChoice and a list of Doubles
 * returns a new NLGroupChoice with rewards set to the list of Doubles
 */
class SetRewards extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType, ListType), WildcardType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    val nlGroupChoice =  args(0).get.asInstanceOf[NLGroupChoice]
    val rewards = args(1).getList.map(_.asInstanceOf[Double]).toList
    return NLGroupChoice(nlGroupChoice.nlAgents, nlGroupChoice.qlAgents, nlGroupChoice.choices, rewards)
  }
}
  
/**
 * takes a turtle- / patchset, the experimenting and the exploration global
 * 
 * new QLAgents are generated for the turtles / patches
 * they are initialised with the parameters and added to the qlDataMap
 */
class Init extends DefaultCommand {
  import QLSystem._
  
  override def getAgentClassString = "O"
  override def getSyntax = commandSyntax(Array( TurtlesetType | PatchsetType, NumberType, StringType))

  def perform(args: Array[Argument], c: Context) {

    betweenTickPerf update PerformanceMeasure()
    handleGroupPerf update PerformanceMeasure()
    guiInterPerf update PerformanceMeasure()

    headlessIdlePerf update Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure()) 
    headlessHandleNLGroupPerf update Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure())
    headlessHandleNLGroupChoicePerf update Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure())
  
    headlessAnswerNLPerf update PerformanceMeasure()
      
    // the groups structure is deleted, schedulers are cancelled, 
    // the NetLogo-model is reloaded and the reward-reporter is recompiled
    netLogoSuper ! NetLogoSupervisor.InitNetLogoActors
    
    val newNLAgents = args(0).getAgentSet.agents.asScala.toList
    val experimenting = args(1).getDoubleValue
    val exploration = args(2).getString

    // create a QLData-object that holds all the information about the Q-learning process
    qlDataMap send { newNLAgents.map(a => {
      a -> AkkaAgent(QLAgent(exploration, experimenting, a.asInstanceOf[org.nlogo.agent.Agent]))
    }).toMap }
    // must wait for new agents to be set
    qlDataMap.await
  }
}

/**
 * takes an agent-set and a list of alternatives
 * sets the same list of alternatives to each agent
 * returns an object of type NLGroup
 */
class CreateGroup extends DefaultReporter {

  override def getAgentClassString = "OTPL"
  override def getSyntax = reporterSyntax(Array( ListType), WildcardType)
  
  def report(args: Array[Argument], context: Context): AnyRef = {
    
    val (nlAgents, qlAgents, alternatives) = args(0).getList.map(entry => {
      val ll = entry.asInstanceOf[LogoList]
      val nlAgent = ll.first.asInstanceOf[org.nlogo.api.Agent]
      val alt = ll.butFirst.first.asInstanceOf[LogoList].map(s => s.asInstanceOf[String]).toList
      
      // find QLAgent to NLAgent
      val qlAgent = QLSystem.qlDataMap.get().apply(nlAgent)
      qlAgent send { _.setAlternatives(alt) }
      
      (nlAgent, qlAgent, alt)
    }).toList.unzip3
       
    NLGroup(nlAgents, qlAgents, alternatives)
  }
}

/**
 * takes a list of objects of type NLGroup 
 * this list is taken as fixed group structure in the simulation run
 */
class NewGroupStructure extends DefaultCommand {
  
  override def getAgentClassString = "O"
  override def getSyntax = commandSyntax(Array(ListType))
  
  def perform(args: Array[Argument], c: Context) {
    val groupStructure = args(0).getList.map(_.asInstanceOf[NLGroup]).toList
    QLSystem.netLogoSuper ! NetLogoSupervisor.SetGroupStructure(groupStructure)
  }
}

/**
 * starts the simulation
 * 
 * if no group structure has been set, the group-reporter is called repeatedly 
 * in order to get a list of objects of type NLGroup 
 */
class Start extends DefaultCommand {
  
  override def getAgentClassString = "O"
  override def getSyntax = commandSyntax(Array[Int]())
  
  def perform(args: Array[Argument], c: Context) {
    QLSystem.netLogoSuper ! NetLogoSupervisor.Start
  }
}

/**
 * stops the simulation
 * 
 * can be restarted
 */
class Stop extends DefaultCommand {
    
  override def getAgentClassString = "O"
  override def getSyntax = commandSyntax(Array[Int]())
  
  def perform(args: Array[Argument], c: Context) {
    QLSystem.netLogoSuper ! NetLogoSupervisor.Stop
  }
}

/**
 * tells all agent to start decreasing the experimentation
 */
class DecreaseExperimenting extends DefaultCommand {

  override def getAgentClassString = "O"
  override def getSyntax = commandSyntax(Array[Int](NumberType))
  
  def perform(args: Array[Argument], c: Context) {
    QLSystem.qlDataMap.get.values.foreach(_ send {_.startDecreasing(args(0).getDoubleValue)} )
  }
}


class GetPerformance extends DefaultReporter {
  override def getAgentClassString = "O"    
  override def getSyntax = reporterSyntax(Array[Int](StringType), NumberType)
  def report(args: Array[Argument], c: Context): AnyRef = { 
    args(0).getString match {
      case "NLSuperBetweenTick" =>
        QLSystem.betweenTickPerf.get.average.toLogoObject
      case "NLSuperHandleGroups" => 
        QLSystem.handleGroupPerf.get.average.toLogoObject
      case "NLSuperGuiInter" =>
        QLSystem.guiInterPerf.get.average.toLogoObject
      case "HeadlessIdlePerf 1" => 
        QLSystem.headlessIdlePerf.get.apply(0).average.toLogoObject
      case "HeadlessIdlePerf 2" => 
        QLSystem.headlessIdlePerf.get.apply(1).average.toLogoObject
      case "HeadlessIdlePerf 3" => 
        QLSystem.headlessIdlePerf.get.apply(2).average.toLogoObject
      case "HeadlessIdlePerf 4" => 
        QLSystem.headlessIdlePerf.get.apply(3).average.toLogoObject
      case "HeadlessHandleNLGroupPerf 1" => 
        QLSystem.headlessHandleNLGroupPerf.get.apply(0).average.toLogoObject
      case "HeadlessHandleNLGroupPerf 2" => 
        QLSystem.headlessHandleNLGroupPerf.get.apply(1).average.toLogoObject
      case "HeadlessHandleNLGroupPerf 3" => 
        QLSystem.headlessHandleNLGroupPerf.get.apply(2).average.toLogoObject
      case "HeadlessHandleNLGroupPerf 4" => 
        QLSystem.headlessHandleNLGroupPerf.get.apply(3).average.toLogoObject
      case "HeadlessHandleNLGroupChoicePerf 1" => 
        QLSystem.headlessHandleNLGroupChoicePerf.get.apply(0).average.toLogoObject
      case "HeadlessHandleNLGroupChoicePerf 2" => 
        QLSystem.headlessHandleNLGroupChoicePerf.get.apply(1).average.toLogoObject
      case "HeadlessHandleNLGroupChoicePerf 3" => 
        QLSystem.headlessHandleNLGroupChoicePerf.get.apply(2).average.toLogoObject
      case "HeadlessHandleNLGroupChoicePerf 4" => 
        QLSystem.headlessHandleNLGroupChoicePerf.get.apply(3).average.toLogoObject
      case "HeadlessAnswerNLPerf" =>
        QLSystem.headlessAnswerNLPerf.get.average.toLogoObject
        
    }
  }
}



