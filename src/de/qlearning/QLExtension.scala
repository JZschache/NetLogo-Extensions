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
  
  val alteredData = AkkaAgent(List[(Agent,String,Double)]())(system)
  
  val nlguiActor = system.actorOf(Props(new NetLogoGUIActor(App.app, alteredData)).withDispatcher("netlogo-dispatcher"), "NetLogoGUIActor")
  
  val QLEXTENSION = "ql"
    
  var agentMap = Map[Agent, ActorRef]()
  
  
  def init() {
    
  }
}

class QLExtension extends DefaultClassManager {
  import QLSystem._
  
  override def load(manager: PrimitiveManager) {
    manager.addPrimitive("init-environment", new InitEnvironment)
    manager.addPrimitive("start-choice", new StartChoice)
    manager.addPrimitive("stop-choice", new StopChoice)
//    manager.addPrimitive("current-agent", new GetCurrentAgent)
//    manager.addPrimitive("current-alternative", new GetCurrentAlt)
    manager.addPrimitive("update-gui", new UpdateGUI)
    manager.addPrimitive("altered-data", new GetAlteredData)
  }
  
  override def additionalJars: JList[String] = {
    val list : java.util.List[String] =  new java.util.ArrayList[String]
    list.add("akka-actor-2.0.5.jar")
    list.add("config-1.0.2.jar")
    list.add("colt-1.2.0.jar")
    list
  }
  
  override def clearAll() {
    agentMap.values.foreach(pair => {
      system.stop(pair)
//      system.stop(pair._2)
    })
    agentMap = Map[Agent, ActorRef]() 
  }
  
}

class InitEnvironment extends DefaultCommand {
  import QLSystem._
  import StartNetLogo._
  import QLAgent._
  
  override def getAgentClassString = "O"
  
  // takes a turtle- / patchset, the experimenting parameter, the names of the alternatives, 
  // the name of the reward-reporter, the name of the GUI-update command  
  override def getSyntax = commandSyntax(Array( TurtlesetType | PatchsetType, NumberType, ListType, StringType, StringType))
  
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
    
    nlguiActor ! NetLogoActors.Init(args(4).getString)
    
    val modelPath = App.app.workspace.getModelPath()
    
    val environment = system.actorOf(Props(new NetLogoHeadlessActor(modelPath, rewardReporterName)), "Environment")
    
    agentMap = newAgents.map(agent => {
      val a = if (agent.isInstanceOf[org.nlogo.api.Turtle]) 
          system.actorOf(Props(new QLAgent(agent, experimenting)), "QLAgent-" + agent.getVariable(0))
        else // patch
          system.actorOf(Props(new QLAgent(agent, experimenting)), "QLAgent-" + agent.getVariable(0) + "-" + agent.getVariable(1))
      a ! Init(environment)
      (agent -> a)
    }).toMap
    
    
    
//    val e = if (agent.isInstanceOf[org.nlogo.api.Turtle])
//          system.actorOf(Props(new EnvironmentActor(rewardReporterName)), "Environment-" + agent.getVariable(0))
//        else // patch
//          system.actorOf(Props(new EnvironmentActor(rewardReporterName)), "Environment-" + agent.getVariable(0) + "-" + agent.getVariable(1))
//      a ! Init(e)
//    
    agentMap.values.foreach(pair => {
      pair ! AddChoiceAltList(altList, true)
    })
  }
    
}


class StartChoice extends DefaultCommand {
  import QLSystem._
  import QLAgent._
  
  override def getAgentClassString = "O"
  
  override def getSyntax = commandSyntax(Array[Int]())
  
  def perform(args: Array[Argument], c: Context) {
//    val source = c.getAgent
//    if (agentMap.contains(source)) {
//      agentMap(source) ! Start
//    }
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

//class GetCurrentAgent extends DefaultReporter {
//    
//  override def getAgentClassString = "O"
//  
//  override def getSyntax = reporterSyntax(Array[Int](), AgentType)
//  
////  def report(args: Array[Argument], c: Context): AnyRef = currentAgent.get
//    
//}

//class GetCurrentAlt extends DefaultReporter {
//  
//  override def getAgentClassString = "O"
//  
//  override def getSyntax = reporterSyntax(Array[Int](), StringType)
//  
////  def report(args: Array[Argument], c: Context): AnyRef = currentAlt.get
//    
//}


/**
 * the UpdateGUI Message should be send to nlguiActor after all previous updates have been made
 * because it triggers a complete change of the alteredData-Agent
 */
class UpdateGUI extends DefaultCommand {
  override def getAgentClassString = "O"  
  override def getSyntax = commandSyntax(Array[Int]())
  def perform(args: Array[Argument], c: Context) {
    QLSystem.nlguiActor ! NetLogoActors.UpdateGUI    
  }
}
class GetAlteredData extends DefaultReporter {
  override def getAgentClassString = "O"    
  override def getSyntax = reporterSyntax(Array[Int](), ListType)
  def report(args: Array[Argument], c: Context): AnyRef = { 
//    QLSystem.system.scheduler.scheduleOnce(1000.milliseconds, QLSystem.nlguiActor, NetLogoGUIActor.UpdateGUI)
    val r = QLSystem.alteredData.await(1000.milliseconds).reverse.map(f => List(f._1, f._2, f._3))
    r.toLogoList
  }
}
