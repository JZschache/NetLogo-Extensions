package de.qlearning

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy._
import akka.actor.ActorKilledException
import akka.actor.ActorInitializationException
import akka.actor.Props
import akka.actor.Cancellable
import akka.routing.{SmallestMailboxRouter, BroadcastRouter, FromConfig}
import akka.agent.{ Agent => AkkaAgent }
import akka.routing.Broadcast
import akka.actor.FSM
import akka.dispatch.Future
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._

import akka.dispatch.ExecutionContext
//import scala.concurrent.ExecutionContext
import scala.collection.immutable.Queue
import java.util.concurrent.Executors


object NetLogoActors {
  
//  case class Init(commandName: String)
//  case object UpdateGUI
//  case object ReturnData
//  case class AlteredAgent(agent: Agent, alt: String, reward: Double)
  
  val supervisorName = "NetLogoSupervisor"
  val helperName = "NetLogoHelper"
//  val routeeSuperName = "NetLogoRouteeSuper"
  val routerName = "NetLogoActorsRouter"
  val dispatcherName = "netlogo-dispatcher"
    
  val config = QLSystem.system.settings.config
  val cfgstr = "netlogo"
  
  // states
  sealed trait State
  case object Idle extends State
  case object Supervising extends State
  // data
  sealed trait Data
  case object Uninitialized extends Data
  case object Initialized extends Data
  case class WithGroupStructure(groups: ActorRef) extends Data
  case class WithGroupStructureAndScheduler(groups: ActorRef, scheduler: Cancellable) extends Data
  case class WithGroupReporter(groups: ActorRef, groupReporter: org.nlogo.nvm.Procedure, scheduler: Cancellable) extends Data
//  case class Initialized(router: ActorRef) extends Data
//  case class WithGroupStructure(router: ActorRef, groups: ActorRef) extends Data
//  case class WithGroupStructureAndScheduler(router: ActorRef, groups: ActorRef, scheduler: Cancellable) extends Data
//  case class WithGroupReporter(router: ActorRef, groups: ActorRef, groupReporter: org.nlogo.nvm.Procedure, scheduler: Cancellable) extends Data
  //messages
  case object InitNetLogoActors
  case class SetGroupStructure(structure: List[NLGroup])
  case object Start
  case object Stop
  case object CompileReporter
//  case class HandleGroup(group: QLGroup)
//  case class HandleGroups(groups: List[QLGroup])
  case class GroupChoice(groupChoice: NLGroupChoice)
  
//  case object GetIdFromHelper
//  case class IdFromHelper(id: Int)
//  case class Update(id: Int, param: List[(org.nlogo.api.Agent,String)])
  case class GetGroupChoice
  
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
class NetLogoSupervisor(netLogoRouter: ActorRef) extends Actor with FSM[NetLogoActors.State, NetLogoActors.Data]{
  import NetLogoActors._
  
  val nlApp = org.nlogo.app.App.app
//  val conHeadlessEnv = config.getInt(cfgstr + ".concurrent-headless-environments")
//  val rewardRepName = config.getString(cfgstr + ".reward-reporter-name")
  val groupRepName = config.getString(cfgstr + ".group-reporter-name")
//  val useGroupRep = config.getBoolean(cfgstr + ".use-group-reporter")
  val groupNo = config.getInt(cfgstr + ".group-number")
  val pauseMS = config.getInt(cfgstr + ".group-pause-ms") 
  val batchSize = config.getInt(cfgstr + ".group-batch-size")
  
  //private messages
  case object Tick
//  case object GetChildren
//  case class Children(children: Iterable[ActorRef])
  
//  override val supervisorStrategy = OneForOneStrategy() {
//    case _: ActorKilledException => Escalate
//    case _: ActorInitializationException => Escalate
//    case _ => Resume
//  }
  
//  override def preStart() {
//    
//    // a child that starts (and restarts) the NetLogoHeadlessActors with a pinned dispatcher
//    // these actors are returned by a GetChildren-Children(children)-conversation 
//    context.actorOf(Props(new Actor {
//      
//      override val supervisorStrategy = OneForOneStrategy() {
//        case _: ActorKilledException => Escalate
//        case _: ActorInitializationException => Escalate
//        case _ => Restart
//      }
//      
//      override def preStart()  {
//        (1 to conHeadlessEnv).foreach(id => {
//          val agent = AkkaAgent[List[(org.nlogo.api.Agent,String)]](List())(context.system)
//          helper.setParameter(id, agent)
////          context.actorOf(Props(new NetLogoHeadlessActor(id, agent)).withDispatcher("netlogo-dispatcher"))
//          context.actorOf(Props(new NetLogoHeadlessActor(id, agent)))
//        })
//      }
//      
//      def receive = {
//        case GetChildren =>
//          sender ! Children(context.children)
//      }
//    }), routeeSuperName)
//    
//    
//    
//  }
  
  startWith(Idle, Uninitialized)
  
  when(Idle) {
    // no router yet
    case Event(InitNetLogoActors, data) => {
      data match {
        case Uninitialized => // do nothing
//        case Initialized(router: ActorRef) =>
//          router
        case WithGroupStructure(groups) =>
          context.system.stop(groups)
        case WithGroupStructureAndScheduler(groups, scheduler) =>
          context.system.stop(groups)
          if (!scheduler.isCancelled)
            scheduler.cancel
        case WithGroupReporter(groups, groupReporter, scheduler) => 
          context.system.stop(groups)
          if (!scheduler.isCancelled)
            scheduler.cancel
      }
//      val routeeSuper = context.actorFor(routeeSuperName)
//      routeeSuper ! GetChildren
      netLogoRouter ! Broadcast(CompileReporter)
//      stay using Initialized(router)
      stay using Initialized
    }
    
    // routees arrive
//    case Event(Children(routees: Iterable[ActorRef]), Uninitialized(structure)) =>
//      // create router
//      val router = context.actorOf(Props().withRouter(SmallestMailboxRouter(routees.toSeq)), routerName)
//      // compile the reward- and group-reporter
//      router ! Broadcast(CompileReporter(nlApp.workspace.getModelPath(), rewardRepName))
//      if (useGroupRep) {
//        val groupReporter = nlApp.workspace.compileReporter(groupRepName)
//        stay using Initialized(router, structure, groupReporter)
//      } else {
//        stay using Initialized(router, structure, null)
//      }
      
    // router exists already 
//    case Event(InitNetLogoActors, Initialized(router: ActorRef, _, _)) =>
//      // recompile the reward- and group-reporter
//      router ! Broadcast(CompileReporter(nlApp.workspace.getModelPath(), rewardRepName))
//      if (useGroupRep) {
//        val groupReporter = nlApp.workspace.compileReporter(groupRepName)
//        stay using Initialized(router, Nil, groupReporter)  
//      } else {
//        stay using Initialized(router, Nil, null)
//      }
      
//    case Event(SetGroupStructure(structure: List[NLGroup]), Uninitialized(_)) =>
//      stay using Uninitialized(groups)
      
    // SetGroupStructure message only works if Initialized: router needed
    case Event(SetGroupStructure(structure: List[NLGroup]), Initialized) => {
      println("setGroupStructure")
      val seed = scala.compat.Platform.currentTime.toInt
      val groupsNumber = Math.ceil(structure.length.toDouble / batchSize.toDouble).toInt
      println("groupsNumber: " + groupsNumber)
      val routees = (1 to groupsNumber).foldLeft((structure, List[ActorRef]()))((pair, id) => {
        val (front, tail) = pair._1.splitAt(batchSize)
        val routee = context.actorOf(Props(new GroupHandler(netLogoRouter, seed + id, front)))
        (tail, routee :: pair._2)
      })._2
//      val routees = structure.map(nlgroup => {
//        seed += 1
//        context.actorOf(Props(new GroupActor(router, seed, nlgroup)))
//      })
      val groups = context.actorOf(Props[GroupHandler].withRouter(BroadcastRouter(routees)))
      stay using WithGroupStructure(groups)
    }
    
    // start without fixed GroupStructure
    case Event(Start, Initialized) => {
      val seed = scala.compat.Platform.currentTime.toInt
      val routees = (1 to groupNo).map(id => {
        context.actorOf(Props(new GroupHandler(netLogoRouter, seed + id, Nil)))
      })
      val groups = context.actorOf(Props[GroupHandler].withRouter(SmallestMailboxRouter(routees)))
      val groupReporter = nlApp.workspace.compileReporter(groupRepName)
      val scheduler = context.system.scheduler.schedule(pauseMS.milliseconds, pauseMS.milliseconds, self, Tick)
      goto(Supervising) using WithGroupReporter(groups, groupReporter, scheduler)
    }
    
    // start and stop with fixed GroupStructure
    case Event(Start, WithGroupStructure(groups)) => {
      println("Start WithGroupStructure")
//      groups ! QLAgent.Start(pauseMS)
      val scheduler = context.system.scheduler.schedule(pauseMS.milliseconds, pauseMS.milliseconds, groups, QLAgent.Tick)
      stay using WithGroupStructureAndScheduler(groups, scheduler)
    }
    case Event(Stop, WithGroupStructureAndScheduler(groups, scheduler)) => {
      scheduler.cancel
      stay using WithGroupStructure(groups)
    }
      
  }
  
//  onTransition {
//    case Idle -> Supervising =>
//      self ! Tick
//  }
  
//  var start = 0
  // in this state: the NetLogoSupervisor repeatedly calls the groupReporter
  // and distributes the nlGroups to the groups-router
  when(Supervising) {
    case Event(Tick, WithGroupReporter(groups, groupReporter, scheduler)) => {
      
//      println("wait till next tick: " + (scala.compat.Platform.currentTime.toInt - start))
//      start = scala.compat.Platform.currentTime.toInt
      
      val nlgroups = nlApp.workspace.runCompiledReporter(nlApp.owner, groupReporter).asInstanceOf[org.nlogo.api.LogoList].map(_.asInstanceOf[NLGroup]).toList
      
      var (front, tail) = nlgroups.splitAt(batchSize)
      groups ! QLAgent.HandleGroups(front)
      while (!tail.isEmpty) {
        val x = tail.splitAt(batchSize)
        groups ! QLAgent.HandleGroups(x._1)
        tail = x._2
      }
      
//      println("runcompiler: " + (scala.compat.Platform.currentTime.toInt - start))
//      start = scala.compat.Platform.currentTime.toInt
      
      //speed is a number between -110 (very slow) and 110 (very fast) 
//      val speed = nlApp.workspace.speedSliderPosition()
//      context.system.scheduler.scheduleOnce(((speed - 110) * (-1)).milliseconds, self, Tick)
      stay
    }
    case Event(Stop, WithGroupReporter(groups, groupReporter, scheduler)) => {
      scheduler.cancel
      goto(Idle)
    }
  }
  
  whenUnhandled {
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

class NetLogoHeadlessActor(val id: Int) extends Actor {
  import NetLogoActors._
  import QLSystem._
  import org.nlogo.headless.HeadlessWorkspace
  
  // use the (pinned) netlogo-dispatcher as execution environment of the futures
  implicit val ec = context.dispatcher
//  implicit val ec = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
  
  val workspace = HeadlessWorkspace.newInstance
  val rewardRepName = config.getString(cfgstr + ".reward-reporter-name")
//  val helper = context.actorFor("/user/" + helperName)
  
//  override def preStart() {
//    //ask for id and helperAgent
//    helper ! GetIdFromHelper
//  }
  
  override def postStop() {
    workspace.dispose()
  }
  
  // not read to handle any requests, because no Id nor Helper present
  def idle: Receive = {
    
//    case IdFromHelper(id) => {
//      println("GetIdAndHelper: " + id)
//      workspace.modelOpened = false
//      workspace.open(org.nlogo.app.App.app.workspace.getModelPath())
//      context.become(ready(id, workspace.compileReporter(rewardRepName + " " + id), helper))
//    }
    case CompileReporter => {
      workspace.modelOpened = false
      workspace.open(org.nlogo.app.App.app.workspace.getModelPath())
      context.become(ready(workspace.compileReporter(rewardRepName + " " + id), Queue[NLGroupChoice]()))
    }
    
    // if other messages arrive, send them again in 1 second 
    case anyMessage: Any =>
      println("NetLogoHeadlessActor: unhandled message in idle'" + anyMessage + "'")
//      context.system.scheduler.scheduleOnce(1000.milliseconds, self, anyMessage)
  }
  
  // waiting to load a model and compile a reporter
  def ready(reporter: org.nlogo.nvm.Procedure, data: Queue[NLGroupChoice]) : Receive = {
    
    case GroupChoice(groupChoice) => {
//      println("HeadlessActor " + id + " handles GroupChoice")
//      helper ! Update(id, group zip alternatives)
      Future {
        workspace.runCompiledReporter(workspace.defaultOwner, reporter).asInstanceOf[NLGroupChoice]
      } onSuccess {
        case groupChoice =>
          (groupChoice.qlAgents, groupChoice.choices, groupChoice.results).zipped.foreach((agent, alt, r) => agent send {_.updated(alt, r)})
      }
//      val result = workspace.runCompiledReporter(workspace.defaultOwner, reporter).asInstanceOf[org.nlogo.api.LogoList]
//    (qlAgents, alternatives, result.map(_.asInstanceOf[Double]).toList).zipped.foreach((agent, alt, r) => agent send {_.updated(alt, r)})  
      context.become(ready(reporter, data.enqueue(groupChoice)))
    }
    
    case GetGroupChoice => {
      val (element, newData) = data.dequeue
      sender ! GroupChoice(element)
      context.become(ready(reporter, newData))
    }
    
    case anyMessage: Any =>
      println("NetLogoHeadlessActor: unhandled message in waiting'" + anyMessage + "'")
  }
  
  def receive = idle
    
  // ready to handle groups of (NetLogo-)Agents
//  def ready(id:Int, reporter: org.nlogo.nvm.Procedure, helperAgent: AkkaAgent[List[(org.nlogo.api.Agent,String)]]): Receive = {
    
    // if supervisor assigns a group to this HeadlessActor, it asks every agent to choose an alternative
    // and waits for their responses, which are send to self via the GroupChoice-message
//    case HandleGroup(g) => {
//      implicit val timeout = Timeout(60 seconds)
//      Future.sequence(g.group.map(triple => {
//        val ar = triple._2
//        val future = (ar ? QLAgent.Choose(triple._3, rh)).mapTo[QLAgent.Choice]
//        future.map(f => (triple._1, ar, f))
//      })) onSuccess {
//        case result =>
//          val unzipped = result.unzip3
//          self ! GroupChoice(unzipped._1, unzipped._2, unzipped._3.map(_.alternative))
//      }
//    }
    
//    case HandleGroups(list) => {
//      implicit val timeout = Timeout(60 seconds)
//      list.foreach(g => {
//        Future.sequence(g.group.map(triple => {
//          val ar = triple._2
//          val future = (ar ? QLAgent.Choose(triple._3, rh)).mapTo[QLAgent.Choice]
//          future.map(f => (triple._1, ar, f))
//        })) onSuccess {
//          case result =>
//            val unzipped = result.unzip3
//            self ! GroupChoice(unzipped._1, unzipped._2, unzipped._3.map(_.alternative))
//        }
//      })
//    }
    
    // after a group of Agents has chosen alternatives, the HeadlessWorkspaceInstance is used
    // to calculate the reward, which are returned to the agents
//    case GroupChoice(group, actorRefs, alternatives) => {
//      println("HeadlessActor handles GroupChoice")
//      helperAgent update (group zip alternatives)
//      val result = workspace.runCompiledReporter(workspace.defaultOwner, reporter).asInstanceOf[org.nlogo.api.LogoList]
//      (actorRefs, alternatives, result.map(_.asInstanceOf[Double]).toList).zipped.foreach((ar, alt, r) => ar ! QLAgent.Reward(alt, r))
//    }
    
//  }

}

//class NetLogoHelperActor extends Actor {
//  import NetLogoActors._
//  
//  def withMap(map: Map[Int, List[(org.nlogo.api.Agent,String)]], maxKey: Int): Receive = {
//    case GetIdFromHelper => {
//      val id = maxKey + 1
//      sender ! IdFromHelper(id)
//      context.become(withMap(map.updated(id, List[(org.nlogo.api.Agent,String)]()), id))
//    }
//    case Update(id, param) => {
//      println("Update: " + id)
//      context.become(withMap(map.updated(id, param), maxKey))
//    }
//    case GetParam(id) => {
//      println("GetParam: " + id)
//      sender ! Param(map.getOrElse(id, List[(org.nlogo.api.Agent,String)]()))
//    }
//  }
//  
//  def receive = withMap(Map[Int, List[(org.nlogo.api.Agent,String)]](), 0)
//  
//}