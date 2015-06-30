package de.qlearning

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy._
import akka.actor.ActorKilledException
import akka.actor.ActorInitializationException
import akka.actor.Props
import akka.routing.SmallestMailboxRouter
import akka.agent.{ Agent => AkkaAgent }
import akka.routing.Broadcast
import akka.actor.FSM
import akka.dispatch.Future
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._


object NetLogoActors {
  
//  case class Init(commandName: String)
//  case object UpdateGUI
//  case object ReturnData
//  case class AlteredAgent(agent: Agent, alt: String, reward: Double)
  
  val supervisorName = "NetLogoSupervisor"
  val routeeSuperName = "NetLogoRouteeSuper"
  val routerName = "NetLogoActors"
  // states
  sealed trait State
  case object Idle extends State
  case object Supervising extends State
  // data
  sealed trait Data
  case object Uninitialized extends Data
  case class Initialized(router: ActorRef, groupReporter: org.nlogo.nvm.Procedure) extends Data
  //messages
  case object InitNetLogoActors
  case object Start
  case object Stop
  case class CompileReporter(modelPath: String, rewardReporterName: String)
  case class HandleGroup(group: QLGroup)
  
  
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

/**
 * The NetLogoSupervisor is the main Actor of the Extension.
 * It is responsible for the HeadlessWorkspaces (for parallel execution) and distributes the 
 * groups of agents to these workspaces.
 * 
 * The NetLogoSupervisor needs a RewardProcedureHelper to share data between Akka and NetLogo.
 * It is initiated by sending the InitNetLogoActors message, which loads the nlogo-model in the
 * headless environments and (re)compiles the group- and reward-procedure.
 * The Start and Stop messages start and stop the repeated call of the group-procedure (which result
 * is distributed among the headless workspaces).
 */
class NetLogoSupervisor(helper: RewardProcedureHelper) extends Actor with FSM[NetLogoActors.State, NetLogoActors.Data]{
  import NetLogoActors._
  
  val nlApp = org.nlogo.app.App.app
  val config = context.system.settings.config
  val cfgstr = "netlogo"
  val conHeadlessEnv = config.getInt(cfgstr + ".concurrent-headless-environments")
  val rewardRepName = config.getString(cfgstr + ".reward-reporter-name")
  val groupRepName = config.getString(cfgstr + ".group-reporter-name")
  
  //private messages
  case object Tick
  case object GetChildren
  case class Children(children: Iterable[ActorRef])
  
  override val supervisorStrategy = OneForOneStrategy() {
    case _: ActorKilledException => Escalate
    case _: ActorInitializationException => Escalate
    case _ => Resume
  }
  
  override def preStart() {
    
    // a child that takes starts (and restarts) the NetLogoHeadlessActors with a pinned dispatcher
    // these actors are returned by a GetChildren-Children(children)-conversation 
    context.actorOf(Props(new Actor {
      
      override val supervisorStrategy = OneForOneStrategy() {
        case _: ActorKilledException => Escalate
        case _: ActorInitializationException => Escalate
        case _ => Restart
      }
      
      override def preStart()  {
        (1 to conHeadlessEnv).foreach(id => {
          val agent = AkkaAgent[List[(org.nlogo.api.Agent,String)]](List())(context.system)
          helper.setParameter(id, agent)
          context.actorOf(Props(new NetLogoHeadlessActor(id, agent)).withDispatcher("netlogo-dispatcher"))
        })
      }
      
      def receive = {
        case GetChildren =>
          sender ! Children(context.children)
      }
    }), routeeSuperName)
    
    
    
  }
  
  startWith(Idle, Uninitialized)
  
  when(Idle) {
    // no router yet
    case Event(InitNetLogoActors, Uninitialized) =>
      val routeeSuper = context.actorFor(routeeSuperName)
      routeeSuper ! GetChildren
      stay // wait for routees
    
    // routees arrive
    case Event(Children(routees: Iterable[ActorRef]), Uninitialized) =>
      // create router
      val router = context.actorOf(Props().withRouter(SmallestMailboxRouter(routees.toSeq)), routerName)
      // compile the reward- and group-reporter
      router ! Broadcast(CompileReporter(nlApp.workspace.getModelPath(), rewardRepName))
      val groupReporter = nlApp.workspace.compileReporter(groupRepName)
      
      stay using Initialized(router, groupReporter)
      
    // router exists already 
    case Event(InitNetLogoActors, Initialized(router: ActorRef, _)) =>
      // recompile the reward- and group-reporter
      router ! Broadcast(CompileReporter(nlApp.workspace.getModelPath(), rewardRepName))
      val groupReporter = nlApp.workspace.compileReporter(groupRepName)
      
      stay using Initialized(router, groupReporter)
    
    // Start message only works if router and groupReporter are initialized
    case Event(Start, Initialized(_,_)) =>
      goto(Supervising)
  }
  
  onTransition {
    case _ -> Supervising =>
      self ! Tick
  }
  
  when(Supervising) {
    case Event(Tick, Initialized(router, groupReporter)) =>
      
      val result = nlApp.workspace.runCompiledReporter(nlApp.owner, groupReporter).asInstanceOf[org.nlogo.api.LogoList]
      val r = result.foreach(group => {
        router ! HandleGroup(group.asInstanceOf[QLGroup])
      })
      
      //TODO: use slider-position to control speed 
      println(nlApp.workspace.speedSliderPosition())
      context.system.scheduler.scheduleOnce(100.milliseconds, self, Tick)
      stay
  }
  
  whenUnhandled {
    case Event(Stop, _) =>
      goto(Idle)
    case Event(Tick, _) =>
      stay
  }
  
  initialize
  
}

/**
   * the NetLogoHeadlessActor is started by the Actor 'routeeSuperName'
   * (who is a child of the NetLogoSupervisor and whose main task is to
   * start the HeadlessActors and Restart them on failure).
   * the NetLogoHeadlessActor gets an id and an (Akka)Agent to share data
   * with an instance of the NetLogoHeadlessWorkspace (via the extension).
   * This kind of sharing data has better performance than setting the parameters 
   * directly and recompiling the reward-reporter 
   * (see also: https://groups.google.com/forum/#!msg/netlogo-devel/8oDmCRERDlQ/0IDZm015eNwJ). 
   */

class NetLogoHeadlessActor(id: Int, helperAgent: AkkaAgent[List[(org.nlogo.api.Agent,String)]]) extends Actor {
  import NetLogoActors._
  import QLSystem._
  import org.nlogo.headless.HeadlessWorkspace
  
  // use the (pinned) netlogo-dispatcher as execution environment of the futures
  implicit val ec = context.dispatcher
  
  val workspace = HeadlessWorkspace.newInstance
  
  //private messages
  private case class GroupChoice(group: List[org.nlogo.api.Agent], actorRefs: List[ActorRef], alternatives: List[String])
  
  override def postStop() {
    workspace.dispose()
  }
  
  // not read to handle any requests, because no model has been loaded and no reporter compiled
  def idle: Receive = {
    
    case CompileReporter(modelPath, rewardReporterName) => {
      workspace.open(modelPath)
      context.become(ready(workspace.compileReporter(rewardReporterName + " " + id)) orElse idle)
    }
    
    // if other messages arrive, send them again in 1 second 
    case anyMessage: Any =>
      println("NetLogoHeadlessActor " + id + ": sending again message '" + anyMessage + "'")
      context.system.scheduler.scheduleOnce(1000.milliseconds, self, anyMessage)
  }
  
  // ready to handle groups of (NetLogo-)Agents
  def ready(reporter: org.nlogo.nvm.Procedure): Receive = {
    
    // if supervisor assigns a group to this HeadlessActor, it asks every agent to choose an alternative
    // and waits for their responses, which are send to self via the GroupChoice-message
    case HandleGroup(g) => {
      implicit val timeout = Timeout(60 seconds)
      Future.sequence(g.group.map(pair => {
        val ar = QLSystem.getActorRef(pair._1)
        val future = (ar ? QLAgent.Choose(pair._2)).mapTo[QLAgent.Choice]
        future.map(f => (pair._1, ar, f))
      })) onSuccess {
        case result =>
          val unzipped = result.unzip3
          self ! GroupChoice(unzipped._1, unzipped._2, unzipped._3.map(_.alternative))
      }
    }
    
    // after a group of Agents has chosen alternatives, the HeadlessWorkspaceInstance is used
    // to calculate the reward, which are returned to the agents
    case GroupChoice(group, actorRefs, alternatives) => {
      helperAgent update (group zip alternatives)
      val result = workspace.runCompiledReporter(workspace.defaultOwner, reporter).asInstanceOf[List[Double]]
      (actorRefs, alternatives, result).zipped.foreach((ar, alt, r) => ar ! QLAgent.Reward(alt, r))
    }
    
  }
  
  def receive = idle
}