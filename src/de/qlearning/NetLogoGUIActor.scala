package de.qlearning

import java.awt.EventQueue
import scala.collection.immutable.Stack
import akka.actor.Actor
import org.nlogo.app.App
import org.nlogo.agent.ArrayAgentSet
import org.nlogo.workspace.AbstractWorkspace
import org.nlogo.api.SimpleJobOwner
import org.nlogo.api.Observer
import org.nlogo.api.Agent
import org.nlogo.api.DefaultReporter
import org.nlogo.nvm.Procedure

object NetLogoGUIActor {
  
  case class Init(commandName: String)
  case object UpdateGUI
  case object ReturnData
  case class AlteredAgent(agent: Agent, alt: String, reward: Double)
  
}

//app.workspace.compileCommands(commandName)
//app.workspace.runCompiledCommands(app.owner, command)

class NetLogoGUIActor(nlApp: App, alteredDataAgent: akka.agent.Agent[QLSystem.AgentsChoiceData]) extends Actor {
  import NetLogoGUIActor._
  import QLSystem._
  

  def needsGUIUpdateCommand: Receive = {
    case Init(commandName) =>
      context.become(collectingData(nlApp.workspace.compileCommands(commandName), (List[Agent](),List[String](), List[Double]())))
  }
  
  
  def collectingData(command: Procedure, data: (List[Agent], List[String], List[Double])): Receive = {
    case newEntry: AlteredAgent =>
      context.become(collectingData(command, (newEntry.agent :: data._1, 
          newEntry.alt :: data._2, newEntry.reward :: data._3)))
      
    case Init(commandName) =>
      context.become(collectingData(nlApp.workspace.compileCommands(commandName), (List[Agent](),List[String](), List[Double]())))
      
    case UpdateGUI =>
      alteredDataAgent.update(AgentsChoiceData(data._1, data._2, data._3))
      nlApp.workspace.runCompiledCommands(nlApp.owner, command)
      context.become(collectingData(command, (List[Agent](),List[String](), List[Double]())))
  }
  
  def receive = needsGUIUpdateCommand
  
}