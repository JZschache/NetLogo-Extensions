package de.qlearning


import scala.collection.JavaConverters._

import org.nlogo.api._
import org.nlogo.api.Syntax._
import org.nlogo.api.ScalaConversions._

import de.util.PerformanceMeasure
import de.qlextension.QLExtension
  
import java.net.URLClassLoader
import java.net.URL

import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import akka.actor.Props
import akka.agent.{ Agent => AkkaAgent }
import akka.dispatch.Await
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._

object QLSystem {
  
  // starting the ActorSystem
  implicit val system = ActorSystem("QLSystem", ConfigFactory.parseFile(QLExtension.confFile))
  val config = system.settings.config
  // the timeout is used to wait for NetLogo, especially: GetNLGroupChoices from HeadlessEnvironments, but also: init, clearAll 
  val defaultWaitDuration = config.getInt(QLExtension.cfgstr + ".timeout_ms") milliseconds
  implicit val timeout = Timeout(defaultWaitDuration)
  
  // a number of headless instances of NetLogo are loaded in the background
  // they are used to simultaneousely execute reward procedures
  // a router is used to distribute jobs to the headless instances
  val conHeadlessEnv = config.getInt(QLExtension.cfgstr + ".headless-workspaces")
  val netLogoRouter = system.actorOf(Props().withRouter(NetLogoHeadlessRouter(conHeadlessEnv)))
  
  // the supervisor controls the simulation
  val netLogoSuper = system.actorOf(Props(new NetLogoSupervisor(netLogoRouter)))
      
  // this map connects an NetLogo-agent (turtle / patch) to a QLAgent that performs the learning 
  // it therefore also holds all data about the agents
  val qlDataMap = AkkaAgent(Map[Agent, AkkaAgent[QLAgent]]())
  
  // various AkkaAgents that measure performance of the program
  val hundredTicksPerf = AkkaAgent(PerformanceMeasure())
  val nlSuperIdlePerf = AkkaAgent(PerformanceMeasure())
  val nlSuperHandleGroupsPerf = AkkaAgent(PerformanceMeasure())
  val nlSuperUpdatePerf = AkkaAgent(PerformanceMeasure())
  val headlessIdlePerf = AkkaAgent(Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure()))
  val headlessHandleGroupsPerf = AkkaAgent(Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure()))
  val headlessHandleChoicesPerf = AkkaAgent(Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure()))
  val headlessAnsweringPerf = AkkaAgent(Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure()))
   
  // names of agent variables
  val altListName = QLSystem.config.getString(QLExtension.cfgstr + ".alternatives-list")
  val qvListName = QLSystem.config.getString(QLExtension.cfgstr + ".q-values-list")
  val freqListName = QLSystem.config.getString(QLExtension.cfgstr + ".frequencies-list")
  val explListName = QLSystem.config.getString(QLExtension.cfgstr + ".explorations-list")
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
  def getExtensionName: String = QLExtension.EXTENSION_NAME
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
  def getExtensionName: String = QLExtension.EXTENSION_NAME
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
    
    val id = args(0).getIntValue
    
    headlessAnsweringPerf send {m => m.updated(id, m(id).start(scala.compat.Platform.currentTime))} 
    
    val future = (netLogoRouter ? GetNLGroupChoices(id)).mapTo[NLGroupChoicesList]
    
    // we have to block because NetLogo is waiting for a result
    val result = try {
      Await.result(future, QLSystem.defaultWaitDuration).list
    } catch {
      case e: java.util.concurrent.TimeoutException =>
        println("Timeout when waiting for HeadlessEnvironment with id: " + args(0).getIntValue)
        Nil
    }
    
    headlessAnsweringPerf send {m => m.updated(id, m(id).end(scala.compat.Platform.currentTime))}
      
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
        
    // stop all data agents and reset the map
    // this may take a while
    qlDataMap send {map => {
      map.values.foreach(_.close)
      Map[org.nlogo.api.Agent, AkkaAgent[QLAgent]]()
    }}
    // wait till complete
    qlDataMap.await
    
    // init performance measures
    hundredTicksPerf update PerformanceMeasure()
    nlSuperIdlePerf update PerformanceMeasure()
    nlSuperHandleGroupsPerf update PerformanceMeasure()
    nlSuperUpdatePerf update PerformanceMeasure()
    headlessIdlePerf update Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure()) 
    headlessHandleGroupsPerf update Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure())
    headlessHandleChoicesPerf update Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure())
    
    // the groups structure is deleted, 
    // the NetLogo-model is reloaded,
    // and the commands and reporters are recompiled
    netLogoSuper ! NetLogoSupervisor.InitNetLogoActors(c.asInstanceOf[org.nlogo.nvm.ExtensionContext].workspace())
    
    // read parameters
    val newNLAgents = args(0).getAgentSet.agents.asScala.toList
    val experimenting = args(1).getDoubleValue
    val exploration = args(2).getString

    // create a QLData-object that holds all the information about the Q-learning process
    qlDataMap send { newNLAgents.map(a => {
      val agent = a.asInstanceOf[org.nlogo.agent.Agent]
      // set variables
      val vLength = agent.variables.size
      val idxA = (0 until vLength).toList.find(i => agent.variableName(i) == altListName.toUpperCase())
      val idxQ = (0 until vLength).toList.find(i => agent.variableName(i) == qvListName.toUpperCase())
      val idxN = (0 until vLength).toList.find(i => agent.variableName(i) == freqListName.toUpperCase())
      val idxE = (0 until vLength).toList.find(i => agent.variableName(i) == explListName.toUpperCase())
      if (idxA.isDefined) 
        agent.setVariable(idxA.get, LogoList())
      if (idxQ.isDefined) 
        agent.setVariable(idxQ.get, LogoList())
      if (idxN.isDefined)
        agent.setVariable(idxN.get, LogoList())
      if (idxE.isDefined) 
        agent.setVariable(idxE.get, LogoList())
      // return mapping
      a -> AkkaAgent(QLAgent(exploration, experimenting, agent))
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
    
    val (nlAgents, alternatives) = args(0).getList.map(entry => {
      val ll = entry.asInstanceOf[LogoList]
      val nlAgent = ll.first.asInstanceOf[org.nlogo.api.Agent]
      val alt = ll.butFirst.first.asInstanceOf[LogoList].map(s => s.asInstanceOf[String]).toList 
      (nlAgent, alt)
    }).toList.unzip
       
    NLGroup(nlAgents, Nil, alternatives)
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
    val groupStructure = args(0).getList.map(_.asInstanceOf[NLGroup]).toList.map(group => {
      if (group.qlAgents.isEmpty) {
        // find QLAgents to NLAgents
        // this must be done here because when creating a group, the qlAgents may not exist yet
        val newQlAgents = (group.nlAgents zip group.alternatives).map(pair => {
          val qlAgent = QLSystem.qlDataMap.get().apply(pair._1)
          qlAgent send { _.setAlternatives(pair._2) }
          qlAgent })
        group.copy(qlAgents = newQlAgents)
      } else 
        group
    })
    
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
  override def getSyntax = commandSyntax(Array[Int]())
  
  def perform(args: Array[Argument], c: Context) {
    QLSystem.qlDataMap.get.values.foreach(_ send {_.startDecreasing} )
  }
}


class GetPerformance extends DefaultReporter {
  override def getAgentClassString = "O"    
  override def getSyntax = reporterSyntax(Array[Int](StringType), NumberType)
  def report(args: Array[Argument], c: Context): AnyRef = { 
    val s = args(0).getString
    val strings = s.split(' ')
    if (strings.size == 2)  {
      val id = strings(1).toInt - 1
      strings(0) match {
        case "HeadlessIdle" => 
          QLSystem.headlessIdlePerf.get.apply(id).average.toLogoObject
        case "HeadlessHandleGroups" => 
          QLSystem.headlessHandleGroupsPerf.get.apply(id).average.toLogoObject
        case "HeadlessHandleChoices" => 
          QLSystem.headlessHandleChoicesPerf.get.apply(id).average.toLogoObject
        case "HeadlessAnswering" => 
          QLSystem.headlessAnsweringPerf.get.apply(id).average.toLogoObject
      }
    } else {
      strings(0) match {
        case "NLSuperIdle" =>
          QLSystem.nlSuperIdlePerf.get.average.toLogoObject
        case "NLSuperHandleGroups" => 
          QLSystem.nlSuperHandleGroupsPerf.get.average.toLogoObject
        case "NLSuperUpdate" =>
          QLSystem.nlSuperUpdatePerf.get.average.toLogoObject
        case "HundredTicks" =>
          QLSystem.hundredTicksPerf.get.average.toLogoObject
      }
    }
  }
}



