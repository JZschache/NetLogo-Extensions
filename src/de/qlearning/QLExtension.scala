package de.qlearning

import java.util.{ List => JList }

import scala.collection.JavaConversions._

import akka.actor.ActorSystem
import akka.actor.ActorRef
import akka.actor.Props

import org.nlogo.api._
import org.nlogo.api.Syntax._
import org.nlogo.api.ScalaConversions._

object QLSystem {
  
  val system = ActorSystem("QLSystem")
  
  val QLEXTENSION = "ql"
  val ENVIRONMENT = "environment"
  
  var environment:Option[ActorRef] = None
  var agentMap = Map[Agent, ActorRef]()
  
}

class QLExtension extends DefaultClassManager {
  import QLSystem._
  
  override def load(manager: PrimitiveManager) {
    manager.addPrimitive("init-environment", new InitEnvironment)
    manager.addPrimitive("remove-environment", new RemoveEnvironment)
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
    if (environment.isDefined)
      system.stop(environment.get)
    agentMap.values.foreach(agent => system.stop(agent))
    agentMap = Map[Agent, ActorRef]() 
  }
  
}

class InitEnvironment extends DefaultCommand {
  import QLSystem._
  import QLAgent._
  
  override def getAgentClassString = "O"
  
  // takes a turtle- / patchset, the name of a reporter, and the names of the alternatives
  override def getSyntax = commandSyntax(Array( TurtlesetType | PatchsetType, StringType, ListType))
  
  /**
   * new QLAgents are generated for the turtles / patches
   * they are initialised with the environment and added to the agentMap
   */
  def perform(args: Array[Argument], c: Context) {
    
    val reporterName = args(1).getString
    
    if (environment.isEmpty){
      environment = Some(system.actorOf(Props(new EnvironmentActor(reporterName)), ENVIRONMENT))
      val newAgents = args(0).getAgentSet.agents
      agentMap = newAgents.map(agent => {
        val a = system.actorOf(Props(new QLAgent(agent)), "QLAgent-" + agent.getVariable(0))
        a ! Init(environment.get)
        (agent -> a)
      }).toMap
      
      val altList = args(2).getList.toList.map(ar => ar.asInstanceOf[String])
      agentMap.values.foreach(a => {
        a ! AddChoiceAltList(altList, true)
      })
    }
  }
    
}

class RemoveEnvironment extends DefaultCommand {
  import QLSystem._
  import QLAgent._
  
  override def getAgentClassString = "O"
  
  // takes a turtle- / patchset, the name of a reporter, and the names of the alternatives
  override def getSyntax = commandSyntax(Array[Int]())
  
  def perform(args: Array[Argument], c: Context) {
    
    if (environment.isDefined)
      system.stop(environment.get)
    agentMap.values.foreach(agent => system.stop(agent))
    agentMap = Map[Agent, ActorRef]()
    
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
    val source = c.getAgent.asInstanceOf[org.nlogo.agent.Agent]
    if (agentMap.contains(source)) {
      agentMap(source) ! Stop
    }
    
  }
    
}