package de.qlearning

import java.util.{ List => JList }
import scala.collection.JavaConversions._
import akka.actor.ActorSystem
import akka.actor.ActorRef
import akka.actor.Props
import org.nlogo.api._
import org.nlogo.api.Syntax._
import org.nlogo.api.ScalaConversions._
import org.nlogo.app.App

object QLSystem {
  
  val system = ActorSystem("QLSystem")
  
  val QLEXTENSION = "ql"
    
  var agentMap = Map[Agent, (ActorRef,ActorRef)]()
  
}

class QLExtension extends DefaultClassManager {
  import QLSystem._
  
  override def load(manager: PrimitiveManager) {
    manager.addPrimitive("init-environment", new InitEnvironment)
    manager.addPrimitive("start-choice", new StartChoice)
    manager.addPrimitive("stop-choice", new StopChoice)
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
      system.stop(pair._1)
      system.stop(pair._2)
    })
    agentMap = Map[Agent, (ActorRef,ActorRef)]() 
  }
  
}

class InitEnvironment extends DefaultCommand {
  import QLSystem._
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
      val e = if (agent.isInstanceOf[org.nlogo.api.Turtle])
          system.actorOf(Props(new EnvironmentActor(reporterName)), "Environment-" + agent.getVariable(0))
        else // patch
          system.actorOf(Props(new EnvironmentActor(reporterName)), "Environment-" + agent.getVariable(0) + "-" + agent.getVariable(1))
      a ! Init(e)
      (agent -> (a,e))
    }).toMap
      
    val altList = try {
      args(3).getList.toList.map(ar => ar.asInstanceOf[String])
    } catch {
      case ex:ClassCastException =>   
        args(3).getList.toList.map(ar => ar.asInstanceOf[Double].toString)
    }
    agentMap.values.foreach(pair => {
      pair._1 ! AddChoiceAltList(altList, true)
    })
  }
    
}


class StartChoice extends DefaultCommand {
  import QLSystem._
  import QLAgent._
  
  override def getAgentClassString = "TP"
  
  override def getSyntax = commandSyntax(Array[Int]())
  
  def perform(args: Array[Argument], c: Context) {
    val source = c.getAgent.asInstanceOf[org.nlogo.agent.Agent]
    if (agentMap.contains(source)) {
      agentMap(source)._1 ! Start
    }
  }
}

class StopChoice extends DefaultCommand {
  import QLSystem._
  import QLAgent._
  
  override def getAgentClassString = "TP"
  
  override def getSyntax = commandSyntax(Array[Int]())
  
  def perform(args: Array[Argument], c: Context) {
    val source = c.getAgent.asInstanceOf[org.nlogo.agent.Agent]
    if (agentMap.contains(source)) {
      agentMap(source)._1 ! Stop
    }
    
  }
    
}