package de.qlearning

import java.util.{ List => JList }
import java.io.File
import scala.collection.JavaConversions._
import akka.actor.ActorSystem
import akka.actor.ActorRef
import akka.actor.Props
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

object QLSystem {
  
  val confFile = "conf/application.conf"
  val system = ActorSystem("QLSystem", ConfigFactory.parseFile(new File(confFile)))
  
  val cfgstr = "de.qlearning"
  val config = system.settings.config
  val conHeadEnv = config.getInt(cfgstr + ".concurrent-headless-environments")
  
//  val alteredData = AkkaAgent(List[(Agent,String,Double)]())(system)
  // the guiActor gets an own Thread-Pool for optimal performance
//  val nlguiActor = system.actorOf(Props(new NetLogoGUIActor(App.app)).withDispatcher("netlogo-dispatcher"), "NetLogoGUIActor")
  
  val r = (1 to conHeadEnv).map(i => (i -> AkkaAgent[(Agent,String)]((null, ""))(system))).toMap
  val rewProcHelper: RewardProcedureHelper = new RewardProcedureHelper(r)
  
  val QLEXTENSION = "ql"
    
  var agentMap = Map[Agent, ActorRef]()
  var environmentList = List[ActorRef]()
  var qValuesMap = Map[Agent, AkkaAgent[Map[String,QLAgent.QValue]]]()
  var lastChoiceMap = Map[Agent, AkkaAgent[String]]()
  
  def init() {
    
  }
}

class QLExtension extends DefaultClassManager {
  import QLSystem._
  
  override def load(manager: PrimitiveManager) {
    manager.addPrimitive("init-environment", new InitEnvironment)
    manager.addPrimitive("start-choice", new StartChoice)
    manager.addPrimitive("stop-choice", new StopChoice)
    manager.addPrimitive("env-parameters", rewProcHelper)
//    manager.addPrimitive("update-gui", new UpdateGUI)
    manager.addPrimitive("qvalues", new GetQValues)
    manager.addPrimitive("last-choice", new GetLastChoice)
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
    environmentList.foreach(env => system.stop(env))
    environmentList = List[ActorRef]()
    agentMap.values.foreach(agent => system.stop(agent))
    agentMap = Map[Agent, ActorRef]()
    qValuesMap.values.foreach(_.close)
    qValuesMap = Map[Agent, AkkaAgent[Map[String,QLAgent.QValue]]]()
    lastChoiceMap.values.foreach(_.close)
    lastChoiceMap = Map[Agent, AkkaAgent[String]]()
  }
  
}

class RewardProcedureHelper(val envParameterMap: Map[Int, AkkaAgent[(Agent,String)]]) extends DefaultReporter {
    
//  private var envParameterMap: Map[Int, (Agent, String)] = Map() 
  
  def setParameter(envId: Int, agent: Agent, alt: String) {
//    val print = !envParameterMap.contains(envId)
    envParameterMap(envId) update (agent, alt)
//    envParameterMap = envParameterMap.updated(envId, (agent, alt))
//    if (print) 
//      println(("" /: envParameterMap.keys) (_ + " " + _))
//    return true
  }
  
  override def getAgentClassString = "O"
  
  override def getSyntax = reporterSyntax(Array[Int](NumberType), AgentType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
//    val param = envParameterMap(args(0).getIntValue)
    val param = envParameterMap(args(0).getIntValue).await(Timeout(1.second))
    return List(param._1, param._2).toLogoList
  }
    
}


class InitEnvironment extends DefaultCommand {
  import QLSystem._
  import StartNetLogo._
  import QLAgent._
  
  override def getAgentClassString = "O"
  
  // takes a turtle- / patchset, the experimenting parameter, the names of the alternatives, 
  // the name of the reward-reporter
  override def getSyntax = commandSyntax(Array( TurtlesetType | PatchsetType, NumberType, ListType, StringType))
  
  /**
   * new QLAgents are generated for the turtles / patches
   * they are initialised with the environment and added to the agentMap
   */
  def perform(args: Array[Argument], c: Context) {
    
    val newAgents = args(0).getAgentSet.agents
    val experimenting = args(1).getDoubleValue
    val altList = try {
      args(2).getList.toList.map(ar => ar.asInstanceOf[String])
    } catch {
      case ex:ClassCastException =>   
        args(2).getList.toList.map(ar => ar.asInstanceOf[Double].toString)
    } 
    val rewardReporterName = args(3).getString
    
//    nlguiActor ! NetLogoActors.Init(args(4).getString)
    
    val modelPath = App.app.workspace.getModelPath()
    
    environmentList = (1 to conHeadEnv).map(i => system.actorOf(Props(new NetLogoHeadlessActor(i, modelPath, rewardReporterName, rewProcHelper)).withDispatcher("netlogo-dispatcher"), "NetLogoHeadlessActor-" + i)).toList
    var index = 0
    
    agentMap = newAgents.map(agent => {
      
      val qValueAgent = AkkaAgent(Map[String,QLAgent.QValue]())(system)
      qValuesMap = qValuesMap.updated(agent, qValueAgent)
      val lastChoiceAgent = AkkaAgent("")(system)
      lastChoiceMap = lastChoiceMap.updated(agent, lastChoiceAgent)
      
      val a = if (agent.isInstanceOf[org.nlogo.api.Turtle]) {
        system.actorOf(Props(new QLAgent(agent, experimenting, qValueAgent, lastChoiceAgent)), "QLAgent-" + agent.getVariable(0))
      } else { // is patch
        system.actorOf(Props(new QLAgent(agent, experimenting, qValueAgent, lastChoiceAgent)), "QLAgent-" + agent.getVariable(0) + "-" + agent.getVariable(1))
      }
      
      a ! Init(environmentList(index))
      index = (index + 1) % conHeadEnv
      
      a ! AddChoiceAltList(altList, true)
      
      (agent -> a)
    }).toMap
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
    QLSystem.qValuesMap(c.getAgent).get.foldLeft(List[Any]())((list, pair) => pair._2.value :: list).reverse.toLogoList
  }
}
class GetLastChoice extends DefaultReporter {
  override def getAgentClassString = "TP"    
  override def getSyntax = reporterSyntax(Array[Int](), StringType)
  def report(args: Array[Argument], c: Context): AnyRef = { 
    QLSystem.lastChoiceMap(c.getAgent).get
  }
}
