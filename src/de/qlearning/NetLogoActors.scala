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

object NetLogoActors {
  
  case class Init(commandName: String)
  case object UpdateGUI
  case object ReturnData
  case class AlteredAgent(agent: Agent, alt: String, reward: Double)
  
}

class NetLogoGUIActor(nlApp: App, alteredData: akka.agent.Agent[List[(Agent,String,Double)]]) extends Actor {
  import NetLogoActors._
  import QLSystem._
  
 
  def needsGUIUpdateCommand: Receive = {
    case Init(commandName) =>
      context.become(collectingData(nlApp.workspace.compileCommands(commandName), List[(Agent,String,Double)]()))
  }
  
  def collectingData(command: Procedure, data: List[(Agent,String,Double)]): Receive = {
    case newEntry: AlteredAgent =>
      context.become(collectingData(command, (newEntry.agent, newEntry.alt, newEntry.reward) :: data))
      
    case Init(commandName) =>
      context.become(collectingData(nlApp.workspace.compileCommands(commandName), List[(Agent,String,Double)]()))
      
    case UpdateGUI =>
      alteredData.update(data)
      nlApp.workspace.runCompiledCommands(nlApp.owner, command)
      context.become(collectingData(command, List[(Agent,String,Double)]()))
  }
  
  def receive = needsGUIUpdateCommand
  
}

class NetLogoHeadlessActor(modelPath: String, rewardReporterName: String) extends Actor {
  import NetLogoActors._
  import QLSystem._
  
  import org.nlogo.headless.HeadlessWorkspace
  val workspace = HeadlessWorkspace.newInstance
  workspace.open(modelPath)
  val reporter = workspace.compileReporter(rewardReporterName)
 
  override def postStop(): Unit = {
    workspace.dispose()
  }
  
  def receive = {
    case QLAgent.Choice(agent, alternative) => {
      val result = workspace.runCompiledReporter(workspace.defaultOwner, reporter).asInstanceOf[Double]
      println(agent.id + " " + alternative + " " + result)
      nlguiActor ! AlteredAgent(agent, alternative, result)
      sender ! QLAgent.Reward(result)
    }
  }
  
}