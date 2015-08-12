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
  
  val headlessIdlePerf = AkkaAgent(PerformanceMeasure())
  val headlessHandleGroupChoicePerf = AkkaAgent(PerformanceMeasure())
  val headlessAnswerNLPerf = AkkaAgent(PerformanceMeasure())
  
  val mailboxNLGroupsList = AkkaAgent(0)
  val mailboxNLGroupChoicesList = AkkaAgent(0)
  val mailboxGetNLGroupChoices = AkkaAgent(0)
  
  val maxQueueLength = AkkaAgent(0)
  
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
    manager.addPrimitive("set-group-structure", new SetGroupStructure)
    manager.addPrimitive("start", new Start)
    manager.addPrimitive("stop", new Stop)
    manager.addPrimitive("get-group-list", new GetGroupList)
    manager.addPrimitive("get-agents", new GetAgents)
    manager.addPrimitive("get-decisions", new GetDecisions)
    manager.addPrimitive("set-rewards", new SetRewards)
    manager.addPrimitive("decay-exploration", new DecreaseExperimenting)
//    manager.addPrimitive("create-singleton", new CreateSingleton)
    manager.addPrimitive("create-group", new CreateGroup)
    manager.addPrimitive("get-performance", new GetPerformance)
    // agent primitives
    manager.addPrimitive("get-data", new GetData)
    manager.addPrimitive("get-exploration-rate", new GetExperimenting)
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
  import NetLogoActors.{GetNLGroupChoices, NLGroupChoicesList}
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](NumberType), ListType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    
    val time1 = scala.compat.Platform.currentTime
    headlessAnswerNLPerf send { _.start(time1)}
    
    QLSystem.mailboxGetNLGroupChoices send { _ + 1 }
    
    val future = (netLogoRouter ? GetNLGroupChoices(args(0).getIntValue)).mapTo[NLGroupChoicesList]
    // we have to block because NetLogo is waiting for a result
    val result = try {
      Await.result(future, defaultWaitDuration).list
    } catch {
      case e: java.util.concurrent.TimeoutException =>
        
        println("Timeout when waiting for HeadlessEnvironment with id: " + args(0).getIntValue)
        Nil
    }
    
    val time2 = scala.compat.Platform.currentTime
    headlessAnswerNLPerf send { _.end(time2) }
      
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

    headlessIdlePerf update PerformanceMeasure()
    headlessHandleGroupChoicePerf update PerformanceMeasure()
    headlessAnswerNLPerf update PerformanceMeasure()
    
    mailboxNLGroupsList update 0
    mailboxNLGroupChoicesList update 0
    mailboxGetNLGroupChoices update 0
  
    
    // the groups structure is deleted, schedulers are cancelled, 
    // the NetLogo-model is reloaded and the reward-reporter is recompiled
    netLogoSuper ! NetLogoActors.InitNetLogoActors
    
    val newNLAgents = args(0).getAgentSet.agents.asScala
    val experimenting = args(1).getDoubleValue
    val exploration = args(2).getString
 
    // create a QLData-object that holds all the information about the Q-learning process
    qlDataMap send { newNLAgents.map(_ -> AkkaAgent(QLAgent(exploration, experimenting))).toMap}
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
    
    val (agents, alternatives) = args(0).getList.map(entry => {
      val ll = entry.asInstanceOf[LogoList]
      val agent = ll.first.asInstanceOf[org.nlogo.api.Agent]
      val alt = ll.butFirst.first.asInstanceOf[LogoList].map(s => s.asInstanceOf[String]).toList
      (agent, alt)
    }).toList.unzip
       
    NLGroup(agents, Nil, alternatives)
  }
}

/**
 * takes a single agent and a list of alternatives
 * returns an object of type NLGroup (with only one agent)
 */
//class CreateSingleton extends DefaultReporter {
//  
//  override def getAgentClassString = "OTPL"
//  override def getSyntax = reporterSyntax(Array( TurtleType | PatchType, ListType), WildcardType)
//  
//  def report(args: Array[Argument], context: Context): AnyRef = {
//    val agent = args(0).getAgent
//    val alternatives = args(1).getList.map(_.asInstanceOf[String]).toList
//    NLGroup(List(agent), Nil, List(alternatives))
//  }
//}

/**
 * takes a list of objects of type NLGroup 
 * this list is taken as fixed group structure in the simulation run
 */
class SetGroupStructure extends DefaultCommand {
  import QLSystem.netLogoSuper
  import NetLogoActors.SetGroupStructure
  
  override def getAgentClassString = "O"
  override def getSyntax = commandSyntax(Array(ListType))
  
  def perform(args: Array[Argument], c: Context) {
    val groupStructure = args(0).getList.map(_.asInstanceOf[NLGroup]).toList
    
    // find QLAgents to NLAgents
    val map = QLSystem.qlDataMap.get()
    val newGS = groupStructure.mapConserve(nlg => {
      val qlAgents = nlg.nlAgents.map(map(_))
      (qlAgents zip nlg.alternatives) foreach {pair =>
        pair._1 send { _.setAlternatives(pair._2) }
      }
      NLGroup(nlg.nlAgents, qlAgents, nlg.alternatives)
    })
    
    netLogoSuper ! SetGroupStructure(newGS)
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
    QLSystem.netLogoSuper ! NetLogoActors.Start
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
    QLSystem.netLogoSuper ! NetLogoActors.Stop
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
      case "HeadlessIdlePerf" => 
        QLSystem.headlessIdlePerf.get.average.toLogoObject
      case "HeadlessHandleGroupChoicePerf" => 
        QLSystem.headlessHandleGroupChoicePerf.get.average.toLogoObject
      case "HeadlessAnswerNLPerf" => 
        QLSystem.headlessAnswerNLPerf.get.average.toLogoObject
      case "MailboxNLGroupsList" =>
        QLSystem.mailboxNLGroupsList.get.toLogoObject
      case "MailboxNLGroupChoicesList" =>
        QLSystem.mailboxNLGroupChoicesList.get.toLogoObject
      case "MailboxGetNLGroupChoices" => 
        QLSystem.mailboxGetNLGroupChoices.get.toLogoObject
      case "MaxQueueLength" =>
        QLSystem.maxQueueLength.get.toLogoObject
      
    }
  }
}


/**
 * returns a list of triples ('name of alternative', 'q-value', n'), one for each alternative
 * 
 * is called by an agent
 * 
 * this method is non-blocking (not all rewards may have been processed)
 */
class GetData extends DefaultReporter {
  override def getAgentClassString = "TP"    
  override def getSyntax = reporterSyntax(Array[Int](), ListType)
  def report(args: Array[Argument], c: Context): AnyRef = {
//    val key = args(0).getString
//    QLSystem.qlDataMap.get()(c.getAgent).get.qValuesMap.getOrElse(key, new QLAgent.QValue(key, 0.0, 0.0)).value.toLogoObject
    QLSystem.qlDataMap.get()(c.getAgent).get.qValuesMap.elements.map(pair => Array(pair._1, pair._2.value, pair._2.n)).toSeq.toLogoObject
  }
}

/**
 * returns the current experimenting parameter
 * 
 * is called by an agent
 * 
 * this method is non-blocking (not all choices may have been processed)
 */
class GetExperimenting extends DefaultReporter {
  override def getAgentClassString = "TP"    
  override def getSyntax = reporterSyntax(Array[Int](), NumberType)
  def report(args: Array[Argument], c: Context): AnyRef = { 
    QLSystem.qlDataMap.get()(c.getAgent).get.experimenting.toLogoObject
  }
}
