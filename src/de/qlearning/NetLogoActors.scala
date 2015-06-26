package de.qlearning

//import java.awt.EventQueue
//import scala.collection.immutable.Stack
import akka.actor.Actor
//import org.nlogo.app.App
//import org.nlogo.agent.ArrayAgentSet
//import org.nlogo.workspace.AbstractWorkspace
//import org.nlogo.api.SimpleJobOwner
//import org.nlogo.api.Observer
//import org.nlogo.api.Agent
//import org.nlogo.api.DefaultReporter
//import org.nlogo.nvm.Procedure
//import akka.dispatch.Future
//import akka.dispatch.ExecutionContext
//import akka.actor.ActorRef
//import akka.actor.OneForOneStrategy
//import akka.actor.SupervisorStrategy._
//import akka.actor.ActorKilledException
//import akka.actor.ActorInitializationException
//import akka.actor.Props
//import akka.routing.SmallestMailboxRouter
import akka.agent.{ Agent => AkkaAgent }
import akka.util.duration._

object NetLogoActors {
  
//  case class Init(commandName: String)
//  case object UpdateGUI
//  case object ReturnData
//  case class AlteredAgent(agent: Agent, alt: String, reward: Double)
  
  val supervisorName = "NetLogoSupervisor"
  
  case object GetIdAndHelperAgent
  case class IdAndHelperAgent(id: Int, helperAgent: AkkaAgent[List[String]])
  
  case class CompileReporter(modelPath: String, rewardReporterName: String)
  
//  case object GetNetLogoRouter
//  case class NetLogoRouter(router: ActorRef)
}

//class NetLogoGUIActor(nlApp: App, rewardReporterName: String, helper: RewardProcedureHelper) extends Actor {
//  import NetLogoActors._
//  import QLSystem._
//  
//  val reporter = nlApp.workspace.compileReporter(rewardReporterName + " 1")
// 
//  def receive = {
//    case QLGroup.Choices(alternatives) => {
//      
//      helper.setParameter(1, alternatives)
//      val result = nlApp.workspace.runCompiledReporter(nlApp.owner, reporter).asInstanceOf[List[Double]]
//      sender ! QLGroup.Rewards(result)
//    }
//  }
//  
//}

class NetLogoSupervisor(helper: RewardProcedureHelper) extends Actor {
  import NetLogoActors._
  
  def receive = {
    case GetIdAndHelperAgent =>
      val id = 1 + ((0 /: helper.envParameterMap.keys) (Math.max(_,_))) 
      val agent = AkkaAgent[List[String]](List())(context.system)
      helper.setParameter(id, agent)
      sender ! IdAndHelperAgent(id, agent)
  }
  
}

class NetLogoHeadlessActor extends Actor {
  import NetLogoActors._
  import QLSystem._
  import org.nlogo.headless.HeadlessWorkspace
  
  val workspace = HeadlessWorkspace.newInstance
  
  /**
   * the NetLogoHeadlessActor is started by the NetLogoActor-Router
   * when starting, it asks the NetLogoSupervisor for an id and
   * for a (Akka)Agent to share data with an instance of the 
   * NetLogoHeadlessWorkspace (via the extension)
   */
  override def preStart() {
    
    val supervisor = context.actorFor("/" + supervisorName)
    supervisor ! GetIdAndHelperAgent
    
  }
  
  override def postStop() {
    
    workspace.dispose()
    
  }
  
  def noId: Receive = {
    
    case IdAndHelperAgent(id, helperAgent) => 
      context.become(idle(id, helperAgent))
    
    // if other messages arrive, send them again in 1 second 
    case anyMessage: Any => 
      context.system.scheduler.scheduleOnce(1000.milliseconds, self, anyMessage)
  }
  
  def idle(id: Int, helperAgent: AkkaAgent[List[String]]): Receive = {
    
    case CompileReporter(modelPath, rewardReporterName) => {
      workspace.open(modelPath)
      context.become(ready(helperAgent, workspace.compileReporter(rewardReporterName + " " + id)) orElse idle(id, helperAgent))
    }
    
    // if other messages arrive, send them again in 1 second 
    case anyMessage: Any =>
      println("NetLogoHeadlessActor " + id + ": sending again message '" + anyMessage + "'")
      context.system.scheduler.scheduleOnce(1000.milliseconds, self, anyMessage)
    
  }
  
  def ready(helperAgent: AkkaAgent[List[String]], reporter: org.nlogo.nvm.Procedure): Receive = {
    
    case QLGroup.Choices(group, alternatives) => {
      helperAgent update alternatives
      val result = workspace.runCompiledReporter(workspace.defaultOwner, reporter).asInstanceOf[List[Double]]
      group ! QLGroup.Rewards(result)
    }
    
  }
  
  def receive = noId
}