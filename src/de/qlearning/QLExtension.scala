package de.qlearning

import java.util.{ List => JList }
import java.io.File
import scala.collection.JavaConversions._
import akka.actor.ActorSystem
import akka.actor.ActorRef
import akka.actor.Props
import akka.routing.FromConfig
import org.nlogo.api._
import org.nlogo.api.Syntax._
import org.nlogo.api.ScalaConversions._
import org.nlogo.app.App
import com.typesafe.config.ConfigFactory
import akka.dispatch.Await
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._
import akka.agent.{ Agent => AkkaAgent }
import akka.routing.Broadcast

object QLSystem {
  
  val confFile = "conf/application.conf"
  val system = ActorSystem("QLSystem", ConfigFactory.parseFile(new File(confFile)))
  
  val cfgstr = "de.qlearning"
  val config = system.settings.config
//  val conHeadEnv = config.getInt(cfgstr + ".concurrent-headless-environments")
  val pbc = config.getInt(cfgstr + ".pause_between_choice_ms")
  
//  val alteredData = AkkaAgent(List[(Agent,String,Double)]())(system)
  // the guiActor gets an own Thread-Pool for optimal performance
//  val nlguiActor = system.actorOf(Props(new NetLogoGUIActor(App.app)).withDispatcher("netlogo-dispatcher"), "NetLogoGUIActor")
  
//  val noOfEnv = if(conHeadEnv == 0) 1 else conHeadEnv
//  val r = (1 to noOfEnv).map(i => (i -> AkkaAgent[List[String]](List())(system))).toMap
//  val r = (1 to conHeadEnv).map(i => (i -> AkkaAgent[List[String]](List())(system))).toMap
  
  // the helper keeps track of parameters that are set by an NetLogoHeadlessActor and 
  // used from an NetLogoHeadlessApp in reward function calls
  val rewProcHelper: RewardProcedureHelper = new RewardProcedureHelper()
  system.actorOf(Props(new NetLogoSupervisor(rewProcHelper)), NetLogoActors.supervisorName)
  val netLogoActors = system.actorOf(Props[NetLogoHeadlessActor].withRouter(FromConfig()), "NetLogoActors")
  
  val QLEXTENSION = "ql"
    
  var agentMap = Map[Agent, ActorRef]()
  var groupList = List[ActorRef]()
  var groupAgentsMap = Map[Int,List[Agent]]()
//  var environmentList = List[ActorRef]()
//  var envIndex = -1
  var qlDataMap = Map[Agent, AkkaAgent[QLAgent.QLData]]()  
  
  def init() {
    
  }
}

class QLExtension extends DefaultClassManager {
  import QLSystem._
  
  override def load(manager: PrimitiveManager) {
    // observer primitives
    manager.addPrimitive("init-environment", new InitEnvironment)
    manager.addPrimitive("add-group", new AddGroup)
    manager.addPrimitive("start-choice", new StartChoice)
    manager.addPrimitive("stop-choice", new StopChoice)
    manager.addPrimitive("get-decisions", rewProcHelper)
    manager.addPrimitive("dec-exp", new DecreaseExperimenting)
    // agent primitives
    manager.addPrimitive("get-qvalues", new GetQValues)
    manager.addPrimitive("get-last-choice", new GetLastChoice)
    manager.addPrimitive("get-total-n", new GetTotalN)
    manager.addPrimitive("get-ns", new GetNs)
    // group primitives
    manager.addPrimitive("get-agents-of-group", new GetAgentsOfGroup)
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
//    environmentList.foreach(env => system.stop(env))
//    environmentList = List[ActorRef]()
//    envIndex = -1
    groupList.foreach(group => system.stop(group))
    groupList = List[ActorRef]()
    groupAgentsMap = Map[Int, List[Agent]]()
    agentMap.values.foreach(agent => system.stop(agent))
    agentMap = Map[Agent, ActorRef]()
    qlDataMap.values.foreach(_.close)
    qlDataMap = Map[Agent, AkkaAgent[QLAgent.QLData]]()
  }
  
}

class RewardProcedureHelper() extends DefaultReporter {
  
  var envParameterMap = Map[Int, AkkaAgent[List[String]]]()
  
  def setParameter(envId: Int, agent: AkkaAgent[List[String]]) {
    envParameterMap = envParameterMap.updated(envId, agent)
  }
  
  override def getAgentClassString = "O"
  
  override def getSyntax = reporterSyntax(Array[Int](NumberType), ListType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    val param = envParameterMap(args(0).getIntValue).await(Timeout(5.seconds))
    return param.toLogoList
  }
  
}


class InitEnvironment extends DefaultCommand {
  import QLSystem._
  import StartNetLogo._
  import QLAgent._
  
  override def getAgentClassString = "O"
  
  // takes a turtle- / patchset, the group-size parameter, the names of the alternatives, 
  // the name of the reward-reporter, the experimenting and the exploration global
  override def getSyntax = commandSyntax(Array( TurtlesetType | PatchsetType, ListType, StringType, NumberType, StringType))
  
  /**
   * new QLAgents are generated for the turtles / patches
   * they are initialised with the environment and added to the agentMap
   */
  def perform(args: Array[Argument], c: Context) {
    
    val newAgents = args(0).getAgentSet.agents
    val altList = try {
      args(1).getList.toList.map(ar => ar.asInstanceOf[String])
    } catch {
      case ex:ClassCastException =>   
        args(1).getList.toList.map(ar => ar.asInstanceOf[Double].toString)
    }
    val rewardReporterName = args(2).getString
    val experimenting = args(3).getDoubleValue
    val exploration = args(4).getString
    
    netLogoActors ! Broadcast(NetLogoActors.CompileReporter(App.app.workspace.getModelPath(), rewardReporterName))
        
    agentMap = newAgents.map(agent => {
      
      val qlDataAgent = AkkaAgent(new QLAgent.QLData())(system)
      qlDataMap = qlDataMap.updated(agent, qlDataAgent)
      val a = if (agent.isInstanceOf[org.nlogo.api.Turtle]) {
        system.actorOf(Props(new QLAgent(netLogoActors, qlDataAgent)), "QLAgent-" + agent.getVariable(0))
      } else { // is patch
        system.actorOf(Props(new QLAgent(netLogoActors, qlDataAgent)), "QLAgent-" + agent.getVariable(0) + "-" + agent.getVariable(1))
      }
      
      a ! Init(experimenting, exploration)
      a ! AddChoiceAltList(altList, true)
      
      (agent -> a)
    }).toMap
  }
    
}

class AddGroup extends DefaultCommand {
  import QLSystem._
  
  override def getAgentClassString = "O"
  
  override def getSyntax = commandSyntax(Array( TurtlesetType | PatchsetType))
  
  def perform(args: Array[Argument], c: Context) {
     val agents = args(0).getAgentSet.agents
     val g = system.actorOf(Props(new QLGroup(netLogoActors, agents.size)))
     agents.foreach(agent => QLSystem.agentMap(agent) ! QLAgent.AddGroup(g))
  }
  
}


class StartChoice extends DefaultCommand {
  import QLSystem._
  import QLAgent._
  
  override def getAgentClassString = "O"
  
  override def getSyntax = commandSyntax(Array[Int]())
  
  def perform(args: Array[Argument], c: Context) {
    agentMap.values.foreach(_ ! Start)
  }
}

class StopChoice extends DefaultCommand {
  import QLSystem._
  import QLAgent._
  
  override def getAgentClassString = "O"
  
  override def getSyntax = commandSyntax(Array[Int]())
  
  def perform(args: Array[Argument], c: Context) {
    agentMap.values.foreach(_ ! Stop)
  }
    
}

class DecreaseExperimenting extends DefaultCommand {
  import QLSystem._
  import QLAgent._
  
  override def getAgentClassString = "O"
  
  override def getSyntax = commandSyntax(Array[Int]())
  
  def perform(args: Array[Argument], c: Context) {
    agentMap.values.foreach(_ ! DecExp)
  }
}

/**
 * the UpdateGUI Message should be send to nlguiActor after all previous updates have been made
 * because it triggers a complete change of the alteredData-Agent
 */
//class UpdateGUI extends DefaultCommand {
//  override def getAgentClassString = "O"  
//  override def getSyntax = commandSyntax(Array[Int]())
//  def perform(args: Array[Argument], c: Context) {
//    QLSystem.nlguiActor ! NetLogoActors.UpdateGUI    
//  }
//}

//class GetAlteredData extends DefaultReporter {
//  override def getAgentClassString = "O"    
//  override def getSyntax = reporterSyntax(Array[Int](), ListType)
//  def report(args: Array[Argument], c: Context): AnyRef = { 
////    QLSystem.system.scheduler.scheduleOnce(1000.milliseconds, QLSystem.nlguiActor, NetLogoGUIActor.UpdateGUI)
////    val r = QLSystem.alteredData.await(10.seconds).reverse.map(f => List(f._1, f._2, f._3))
//    implicit val timeout = Timeout(10 seconds)
//    val future = QLSystem.nlguiActor ? NetLogoActors.ReturnData
//    val result = Await.result(future, timeout.duration).asInstanceOf[List[(Agent,String,Double)]]
//    println("result is here: " +  result.size + " items")
//    val r = result.reverse.map(f => List(f._1, f._2, f._3))
//    r.toLogoList
//  }
//}

class GetQValues extends DefaultReporter {
  override def getAgentClassString = "TP"    
  override def getSyntax = reporterSyntax(Array[Int](), ListType)
  def report(args: Array[Argument], c: Context): AnyRef = {
    QLSystem.qlDataMap(c.getAgent).get.qValuesMap.foldLeft(List[Any]())((list, pair) => pair._2.value :: list).reverse.toLogoList
  }
}
class GetNs extends DefaultReporter {
  override def getAgentClassString = "TP"    
  override def getSyntax = reporterSyntax(Array[Int](), ListType)
  def report(args: Array[Argument], c: Context): AnyRef = {
    QLSystem.qlDataMap(c.getAgent).get.nMap.foldLeft(List[Any]())((list, pair) => pair._2 :: list).reverse.toLogoList
  }
}
class GetTotalN extends DefaultReporter {
  override def getAgentClassString = "TP"    
  override def getSyntax = reporterSyntax(Array[Int](), NumberType)
  def report(args: Array[Argument], c: Context): AnyRef = { 
    QLSystem.qlDataMap(c.getAgent).get.nTotal.toLogoObject
  }
}
class GetLastChoice extends DefaultReporter {
  override def getAgentClassString = "TP"    
  override def getSyntax = reporterSyntax(Array[Int](), StringType)
  def report(args: Array[Argument], c: Context): AnyRef = { 
    QLSystem.qlDataMap(c.getAgent).get.lastChoice.toLogoObject
  }
}



class GetAgentsOfGroup extends DefaultReporter {
  override def getAgentClassString = "O"    
  override def getSyntax = reporterSyntax(Array[Int](NumberType), ListType)
  def report(args: Array[Argument], c: Context): AnyRef = { 
    QLSystem.groupAgentsMap(args(0).getIntValue).toLogoList
  }
}

