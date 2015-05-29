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


object QLSystem {
  
  val confFile = "conf/application.conf"
  val system = ActorSystem("QLSystem", ConfigFactory.parseFile(new File(confFile)))
  
  case class AgentsChoiceData(agents: List[Agent], alternatives: List[String], rewards: List[Double])
  
  val alteredDataAgent = akka.agent.Agent(AgentsChoiceData(List(), List(), List()))(system)
  
  
  
  val nlguiActor = system.actorOf(Props(new NetLogoGUIActor(App.app, alteredDataAgent)).withDispatcher("netlogo-dispatcher"), "NetLogoGUIActor")
  
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
    manager.addPrimitive("current-agent", new GetCurrentAgent)
    manager.addPrimitive("current-alternative", new GetCurrentAlt)
    manager.addPrimitive("update-gui", new UpdateGUI)
    manager.addPrimitive("altered-agents", new GetAlteredAgents)
    manager.addPrimitive("corr-alternatives", new GetCorrAlternatives)
    manager.addPrimitive("corr-rewards", new GetCorrRewards)
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
  
  // takes a turtle- / patchset, the experimenting parameter, the name of a reporter, and the names of the alternatives
  override def getSyntax = commandSyntax(Array( TurtlesetType | PatchsetType, NumberType, StringType, ListType))
  
  /**
   * new QLAgents are generated for the turtles / patches
   * they are initialised with the environment and added to the agentMap
   */
  def perform(args: Array[Argument], c: Context) {
    
    val experimenting = args(1).getDoubleValue
    val reporterName = args(2).getString
        
    val newAgents = args(0).getAgentSet.agents
    agentMap = newAgents.map(agent => {
      val a = if (agent.isInstanceOf[org.nlogo.api.Turtle]) 
          system.actorOf(Props(new QLAgent(agent, experimenting)), "QLAgent-" + agent.getVariable(0))
        else // patch
          system.actorOf(Props(new QLAgent(agent, experimenting)), "QLAgent-" + agent.getVariable(0) + "-" + agent.getVariable(1))
//      val e = if (agent.isInstanceOf[org.nlogo.api.Turtle])
//          system.actorOf(Props(new EnvironmentActor(reporterName)), "Environment-" + agent.getVariable(0))
//        else // patch
//          system.actorOf(Props(new EnvironmentActor(reporterName)), "Environment-" + agent.getVariable(0) + "-" + agent.getVariable(1))
      a ! Init(netlogo.get)
      (agent -> a)
    }).toMap 
    
    val altList = try {
      args(3).getList.toList.map(ar => ar.asInstanceOf[String])
    } catch {
      case ex:ClassCastException =>   
        args(3).getList.toList.map(ar => ar.asInstanceOf[Double].toString)
    }
    agentMap.values.foreach(pair => {
      pair ! AddChoiceAltList(altList, true)
    })
  }
    
}


class StartChoice extends DefaultCommand {
  import QLSystem._
  import QLAgent._
  
  override def getAgentClassString = "TP"
  
  override def getSyntax = commandSyntax(Array[Int]())
  
  def perform(args: Array[Argument], c: Context) {
    val source = c.getAgent
    if (agentMap.contains(source)) {
      agentMap(source) ! Start
    }
  }
}

class StopChoice extends DefaultCommand {
  import QLSystem._
  import QLAgent._
  
  override def getAgentClassString = "TP"
  
  override def getSyntax = commandSyntax(Array[Int]())
  
  def perform(args: Array[Argument], c: Context) {
    val source = c.getAgent
    if (agentMap.contains(source)) {
      agentMap(source) ! Stop
    }
    
  }
    
}

class GetCurrentAgent extends DefaultReporter {
    
  override def getAgentClassString = "O"
  
  override def getSyntax = reporterSyntax(Array[Int](), AgentType)
  
  def report(args: Array[Argument], c: Context): AnyRef = currentAgent.get
    
}

class GetCurrentAlt extends DefaultReporter {
  
  override def getAgentClassString = "O"
  
  override def getSyntax = reporterSyntax(Array[Int](), StringType)
  
  def report(args: Array[Argument], c: Context): AnyRef = currentAlt.get
    
}


/**
 * the UpdateGUI Message should be send to nlguiActor after all previous updates have been made
 * because it triggers a change in the alteredDataAgent
 */
class UpdateGUI extends DefaultCommand {
  import QLSystem.nlguiActor
  import NetLogoGUIActor.UpdateGUI
  
  override def getAgentClassString = "O"
  
  override def getSyntax = commandSyntax(Array[Int]())
  
  def perform(args: Array[Argument], c: Context) {
    nlguiActor ! UpdateGUI    
  }
}

class GetAlteredAgents extends DefaultReporter {
  import QLSystem._
  import NetLogoGUIActor._
  
  
  override def getAgentClassString = "O"
    
  override def getSyntax = reporterSyntax(Array[Int](), TurtlesetType | PatchsetType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    val result = alteredDataAgent.await(10.seconds)
    result.agents.toLogoList
  }
}
