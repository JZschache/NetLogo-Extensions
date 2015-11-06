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
  
  val defaultState = 0
  
  // starting the ActorSystem
  implicit val system = ActorSystem("QLSystem", ConfigFactory.parseFile(QLExtension.confFile))
  val config = system.settings.config
  // the timeout is used to wait for NetLogo, especially: GetNLGroupChoices from HeadlessEnvironments, but also: init, clearAll 
  val defaultWaitDuration = config.getInt(QLExtension.cfgstr + ".timeout_ms") milliseconds
  implicit val timeout = Timeout(defaultWaitDuration)
  
  val inParallelMode = config.getBoolean("netlogo.enable-parallel-mode")
  val usePerformanceMeasures = if (inParallelMode) config.getBoolean("netlogo.parallel.enable-performance-measures") else false
  
  val netLogoRouter = if (inParallelMode) {
      println("Parallel mode is enabled")
	  // a number of headless instances of NetLogo are loaded in the background
	  // they are used to simultaneousely execute reward procedures
	  // a router is used to distribute jobs to the headless instances
	  val conHeadlessEnv = config.getInt(QLExtension.cfgstr + ".parallel.headless-workspaces")
	  system.actorOf(Props().withRouter(NetLogoHeadlessRouter(conHeadlessEnv)))
	  } else {
	    println("Parallel mode is disabled")
	    system.deadLetters
	  }
  
  val netLogoSuper = if (inParallelMode) {
	  // the supervisor controls the simulation
	  system.actorOf(Props(new NetLogoSupervisor(netLogoRouter)))
	  } else system.deadLetters
  
  // an object to handle the performance measures (works only in parallel-mode)
  val perfMeasures = if (usePerformanceMeasures) new PerformanceMeasures() else new EmptyPerformanceMeasures()
	  
  // this map connects an NetLogo-agent (turtle / patch) to a QLAgent that performs the learning 
  // it therefore also holds all data about the agents
  val qlDataMap = AkkaAgent(Map[Agent, AkkaAgent[QLAgent]]())  
  
  // names of agent variables
  val altListName = QLSystem.config.getString(QLExtension.cfgstr + ".alternatives-list")
  val qvListName = QLSystem.config.getString(QLExtension.cfgstr + ".q-values-list")
  val freqListName = QLSystem.config.getString(QLExtension.cfgstr + ".frequencies-list")
  val explRateName = QLSystem.config.getString(QLExtension.cfgstr + ".exploration-rate")
  val explMethodName = QLSystem.config.getString(QLExtension.cfgstr + ".exploration-method")
  val gammaName = QLSystem.config.getString(QLExtension.cfgstr + ".gamma")
  val stateName = QLSystem.config.getString(QLExtension.cfgstr + ".state")
  
  val defaultExploraionRate = QLSystem.config.getDouble(QLExtension.cfgstr + ".default-exploration-rate")
  val defaultExploraionMethod = QLSystem.config.getString(QLExtension.cfgstr + ".default-exploration-method")
  
  
  def createQLAgent(agent: org.nlogo.agent.Agent)= {
    // read and set variables
	val vLength = agent.variables.size
    val idxA = (0 until vLength).toList.find(i => agent.variableName(i) == altListName.toUpperCase())
    val idxQ = (0 until vLength).toList.find(i => agent.variableName(i) == qvListName.toUpperCase())
    val idxN = (0 until vLength).toList.find(i => agent.variableName(i) == freqListName.toUpperCase())
    val idxER = (0 until vLength).toList.find(i => agent.variableName(i) == explRateName.toUpperCase())
    val idxEM = (0 until vLength).toList.find(i => agent.variableName(i) == explMethodName.toUpperCase())
    val idxG = (0 until vLength).toList.find(i => agent.variableName(i) == gammaName.toUpperCase())
    val idxS = (0 until vLength).toList.find(i => agent.variableName(i) == stateName.toUpperCase())
    val alternatives = if (idxA.isDefined) {
      val entry = agent.getVariable(idxA.get)
      if (entry.isInstanceOf[LogoList])
        entry.asInstanceOf[LogoList].map(e => e.asInstanceOf[Double].toInt).toList
      else {
        agent.setVariable(idxA.get, LogoList())
        List[Int]()
      }
    } else List[Int]()
    if (idxQ.isDefined) 
      agent.setVariable(idxQ.get, LogoList())
    if (idxN.isDefined)
      agent.setVariable(idxN.get, LogoList())
    val explorationRate = if (idxER.isDefined)
        agent.getVariable(idxER.get).asInstanceOf[Double]
      else
        defaultExploraionRate
    val explorationMethod = if (idxEM.isDefined)
        agent.getVariable(idxEM.get).asInstanceOf[String]
      else
        defaultExploraionMethod
    val gamma = if (idxG.isDefined) 
        agent.getVariable(idxG.get).asInstanceOf[Double]
      else
        0.0
    val state = if (idxS.isDefined)
        agent.getVariable(idxS.get).asInstanceOf[Double].toInt
      else
        defaultState
    val akkaAgent = AkkaAgent(QLAgent(state, explorationMethod, explorationRate, gamma, agent))
    akkaAgent send {_.setAlternatives(alternatives)}
	akkaAgent
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
                   alternatives: List[List[Int]]) extends org.nlogo.api.ExtensionObject with hasNLAgents {
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
                         choices: List[Int], 
                         rewards: List[Double],
                         newStates: List[Int]) extends org.nlogo.api.ExtensionObject with hasNLAgents {
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
    
    perfMeasures.startHeadlessAnsweringPerf(id, scala.compat.Platform.currentTime)
        
    val future = (netLogoRouter ? GetNLGroupChoices(id)).mapTo[NLGroupChoicesList]
    
    // we have to block because NetLogo is waiting for a result
    val result = try {
      Await.result(future, QLSystem.defaultWaitDuration).list
    } catch {
      case e: java.util.concurrent.TimeoutException =>
        println("Timeout when waiting for HeadlessEnvironment with id: " + args(0).getIntValue)
        Nil
    }
    
    perfMeasures.stopHeadlessAnsweringPerf(id, scala.compat.Platform.currentTime)
          
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
    return NLGroupChoice(nlGroupChoice.nlAgents, nlGroupChoice.qlAgents, nlGroupChoice.choices, rewards, 
        if (nlGroupChoice.newStates.isEmpty) rewards.map(_ => QLSystem.defaultState) else nlGroupChoice.newStates)
  }
}

/**
 * takes an NLGroupChoice and a list of Integers
 * returns a new NLGroupChoice with new states set to the list of Integers
 */
class SetNewStates extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType, ListType), WildcardType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    val nlGroupChoice =  args(0).get.asInstanceOf[NLGroupChoice]
    val newStates = args(1).getList.map(_.asInstanceOf[Double].toInt).toList
    return NLGroupChoice(nlGroupChoice.nlAgents, nlGroupChoice.qlAgents, nlGroupChoice.choices, nlGroupChoice.rewards, newStates)
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
  override def getSyntax = commandSyntax(Array( TurtlesetType | PatchsetType))

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
    perfMeasures.init
    
    // the groups structure is deleted, 
    // the NetLogo-model is reloaded,
    // and the commands and reporters are recompiled
    netLogoSuper ! NetLogoSupervisor.InitNetLogoActors(c.asInstanceOf[org.nlogo.nvm.ExtensionContext].workspace())
    
    // read parameter
    val newNLAgents = args(0).getAgentSet.agents.asScala.toList
    
    // create the QLAgents
    qlDataMap send { newNLAgents.map(a => a -> createQLAgent(a.asInstanceOf[org.nlogo.agent.Agent])).toMap }
    // must wait for new agents to be set
    qlDataMap.await
  }
}


class AddAgent extends DefaultCommand {
  import QLSystem._
  
  override def getAgentClassString = "O"
  override def getSyntax = commandSyntax(Array( TurtleType | PatchType))

  def perform(args: Array[Argument], c: Context) {
    // read parameter
    val newNLAgent = args(0).getAgent
    // update the QLAgents
    qlDataMap send { _ + (newNLAgent -> createQLAgent(newNLAgent.asInstanceOf[org.nlogo.agent.Agent])) }
    // must wait for new agent to be set
    qlDataMap.await
  }
}

class RemoveAgent extends DefaultCommand {
  import QLSystem._
  
  override def getAgentClassString = "O"
  override def getSyntax = commandSyntax(Array( TurtleType | PatchType))

  def perform(args: Array[Argument], c: Context) {
    // read parameter
    val newNLAgent = args(0).getAgent
    // update the QLAgents
    qlDataMap send { _ - newNLAgent }
    // must wait for new agent to be set
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
      val alt = ll.butFirst.first.asInstanceOf[LogoList].map(s => s.asInstanceOf[Double].toInt).toList
      val qlAgent = QLSystem.qlDataMap.get().get(nlAgent)
      if (qlAgent.isDefined)
        qlAgent.get send { _.setAlternatives(alt) }
      (nlAgent, qlAgent, alt)
    }).toList.unzip3
       
    NLGroup(nlAgents, qlAgents.filter(_.isDefined).map(_.get), alternatives)
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

/**
 * an agent is asked to choose one of the given alternatives
 * 
 * this method is blocking
 */
class OneOf extends DefaultReporter {
  import QLSystem.timeout
  
  override def getAgentClassString = "TP"
  override def getSyntax = reporterSyntax(Array( ListType), NumberType)
  
  def report(args: Array[Argument], context: Context): AnyRef = {
    val alternatives = args(0).getList.map(s => s.asInstanceOf[Double].toInt).toList
    val qlAgent = QLSystem.qlDataMap.get.apply(context.getAgent).await
    Double.box(qlAgent.choose(alternatives))
  }
}

/**
 * a reward is set
 * 
 * the first argument is the previously chosen alternative
 * 
 */
class SetReward extends DefaultCommand {
  
  override def getAgentClassString = "TP"
  override def getSyntax = commandSyntax(Array(NumberType, NumberType))
  
  def perform(args: Array[Argument], context: Context) {
    val alternative = args(0).getIntValue
    val reward = args(1).getDoubleValue
    QLSystem.qlDataMap.get.apply(context.getAgent) send { _.updated(alternative, reward, QLSystem.defaultState)}
  }
}

/**
 * a reward and new state is set 
 * 
 * the first argument is the previously chosen alternative
 * 
 */
class SetRewardAndState extends DefaultCommand {
  
  override def getAgentClassString = "TP"
  override def getSyntax = commandSyntax(Array(NumberType, NumberType, NumberType))
  
  def perform(args: Array[Argument], context: Context) {
    val alternative = args(0).getIntValue
    val reward = args(1).getDoubleValue
    val newState = args(2).getIntValue
    //println("agent " + context.getAgent.id + " chose alternative " + alternative + " and ends up in state " + newState)
    QLSystem.qlDataMap.get.apply(context.getAgent) send { _.updated(alternative, reward, newState)}
  }
}


