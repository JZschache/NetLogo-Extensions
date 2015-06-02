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
import akka.dispatch.Future

import akka.dispatch.ExecutionContext
import akka.actor.ActorRef

object NetLogoActors {
  
  case class Init(commandName: String)
  case object UpdateGUI
  case object ReturnData
  case class AlteredAgent(agent: Agent, alt: String, reward: Double)
  
}

class NetLogoGUIActor(nlApp: App, rewardReporterName: String, helper: RewardProcedureHelper) extends Actor {
  import NetLogoActors._
  import QLSystem._
  
  val reporter = nlApp.workspace.compileReporter(rewardReporterName + " 1")
 
  def receive = {
    case QLGroup.Choices(alternatives) => {
      
      helper.setParameter(1, alternatives)
      val result = nlApp.workspace.runCompiledReporter(nlApp.owner, reporter).asInstanceOf[List[Double]]
      sender ! QLGroup.Rewards(result)
    }
  }
  
}

class NetLogoHeadlessActor(id: Int, modelPath: String, rewardReporterName: String, helper: RewardProcedureHelper) extends Actor {
  import NetLogoActors._
  import QLSystem._
  
  import org.nlogo.headless.HeadlessWorkspace
  val workspace = HeadlessWorkspace.newInstance
  workspace.open(modelPath)
  val reporter = workspace.compileReporter(rewardReporterName + " " + id)
  
  override def postStop(): Unit = {
    workspace.dispose()
  }
  
  def receive = {
    case QLGroup.Choices(alternatives) => {
      helper.setParameter(1, alternatives)
      val result = workspace.runCompiledReporter(workspace.defaultOwner, reporter).asInstanceOf[List[Double]]
      sender ! QLGroup.Rewards(result)
    }
  }
  
}