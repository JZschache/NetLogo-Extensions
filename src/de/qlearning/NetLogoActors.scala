package de.qlearning

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.immutable.Queue
import akka.actor.{Actor,ActorRef,FSM,SupervisorStrategy,Props}
import akka.agent.{Agent => AkkaAgent}
import akka.routing.{Broadcast,Route,RouteeProvider,RouterConfig,Destination}
import akka.dispatch.{Future,Dispatchers}
import akka.util.duration._
import de.util.PerformanceMeasure
import org.nlogo.app.ModelSaver


object NetLogoActors {
  
  // states
  sealed trait State
  case object Idle extends State
  case object Supervising extends State
  // data
  sealed trait Data
  case object Uninitialized extends Data
  case object Initialized extends Data
  case class WithBatchStructure(batches: List[List[NLGroup]]) extends Data
  case class WithGroupReporter(groupReporter: org.nlogo.nvm.Procedure) extends Data
  //messages
  case object InitNetLogoActors
  case class SetGroupStructure(structure: List[NLGroup])
  case object Start
  case object Stop
  case object CompileReporter
  case class NLGroupChoicesList(list: List[NLGroupChoice])
  case class GetNLGroupChoices(id: Int)
  
}


/**
 * The NetLogoSupervisor is the main Actor of the Extension.
 * 
 * It is responsible for the static or dynamic group-structure.
 * 
 * The NetLogoSupervisor needs the NLHeadlessActor-router to send them a CompileReporter message.
 * 
 * It is initiated by sending the InitNetLogoActors message, which resets the group-structure,
 * loads the nlogo-model in the headless environments and lets them (re)compile their reward-procedures.
 * 
 * The Start and Stop messages start and stop the simulation. This means that either the groups of 
 * a static group structure repeatedly send GroupChoices to NetLogo (and to the HeadlessActors) or
 * that the NetLogoSupervisor repeatedly calls the group-procedure, which results in a list of groups that
 * once send GroupChoices to NetLogo ( and the HeadlessActors).
 */
class NetLogoSupervisor(netLogoRouter: ActorRef,
    betweenTickPerf: AkkaAgent[PerformanceMeasure],
    handleGroupPerf: AkkaAgent[PerformanceMeasure],
    guiInterPerf: AkkaAgent[PerformanceMeasure]) extends Actor with FSM[NetLogoActors.State, NetLogoActors.Data]{
  import NetLogoActors._
  import QLSystem._
  
  val nlApp = org.nlogo.app.App.app
    
  val groupRepName = config.getString(cfgstr + ".group-reporter-name")
  val batches = config.getInt(cfgstr + ".batches")
  val updateComName = config.getString(cfgstr + ".update-command-name")
//  val delayDuration = config.getInt(cfgstr + ".delay-ms") milliseconds
  
  //private message
  case object Tick
  
  // function that sets a future for the decisions of a list of groups
  private def handleGroups(groups: List[NLGroup]) = {
//    println("groups: " + groups.size)
    // wait for choice of agents until all updates (QValues) have been placed
    Future.sequence(groups.map(group => Future.sequence((group.qlAgents zip group.alternatives).map(pair => 
      pair._1.future map {_.choose(pair._2)}
    )))) onSuccess {
      case list =>  {
//        println("choices: " + list.size)
        val groupsChoices = (groups zip list).map(pair => NLGroupChoice(pair._1.nlAgents, pair._1.qlAgents, pair._2, Nil))
        netLogoRouter ! NLGroupChoicesList(groupsChoices)
      }
    }
  }
  
  startWith(Idle, Uninitialized)
  
  when(Idle) {
    case Event(InitNetLogoActors, _) => {
      // save changes
      val ms = new ModelSaver(nlApp)
      org.nlogo.api.FileIO.writeFile(nlApp.workspace.getModelPath(), ms.save)
      // the NetLogo-model is reloaded and the reward-reporter is recompiled
      netLogoRouter ! Broadcast(CompileReporter)
      // the groups structure is deleted
      stay using Initialized
    }
      
    // SetGroupStructure message only works if Initialized
    case Event(SetGroupStructure(structure: List[NLGroup]), Initialized) => {
      val batchSize = Math.ceil(structure.size.toDouble / batches.toDouble).toInt
      val batchStructure = (1 to batches).foldLeft((structure, List[List[NLGroup]]())) { (pair, i) =>
        val (front, tail) = pair._1.splitAt(batchSize)  
        (tail, front :: pair._2)
      }._2
      stay using WithBatchStructure(batchStructure)
    }
    
    // start without fixed GroupStructure
    // the group-reporter is called repeatedly 
    // in order to get a list of objects of type NLGroup 
    case Event(Start, Initialized) => {
      val groupReporter = nlApp.workspace.compileReporter(groupRepName)
      self ! Tick
      goto(Supervising) using WithGroupReporter(groupReporter)
    }
    
    // start with fixed GroupStructure
    case Event(Start, WithBatchStructure(batchStructure)) => {
      self ! Tick
      goto(Supervising)
    }
      
  }
  
  // in this state: the NetLogoSupervisor uses the batchStructure or the groupReporter
  // to repeatedly call the handleGroups-function
  when(Supervising) {
    case Event(Tick, data) => {
      
      val time1 = scala.compat.Platform.currentTime
      betweenTickPerf send { _.end(time1) }
      handleGroupPerf send { _.start(time1)}
      
      data match {
        case WithBatchStructure(batchStructure) =>
//          println("WithBatchStructure(batchStructure) ")
          batchStructure.foreach(batch => handleGroups(batch))
        case WithGroupReporter(groupReporter) => {
          val nlgroups = nlApp.workspace.runCompiledReporter(nlApp.owner, groupReporter).asInstanceOf[org.nlogo.api.LogoList].map(_.asInstanceOf[NLGroup]).toList
          if (batches > 1) {
            val batchSize = Math.ceil(nlgroups.size.toDouble / batches.toDouble).toInt
            var (front, tail) = nlgroups.splitAt(batchSize)
            handleGroups(front)
            while (!tail.isEmpty) {
              val x = tail.splitAt(batchSize)
              handleGroups(x._1)
              tail = x._2
            }
          } else handleGroups(nlgroups) 
        }
        case _ => // do nothing
      }
      
      val time2 = scala.compat.Platform.currentTime
      handleGroupPerf send { _.end(time2)}
      guiInterPerf send { _.start(time2) }
      
      nlApp.command(updateComName)
      
      //speed is a number between -110 (very slow) and 110 (very fast) 
      val speed = nlApp.workspace.speedSliderPosition()
      if (speed == 110) {
        self ! Tick
      } else
        context.system.scheduler.scheduleOnce(((speed - 110) * (-2)).milliseconds, self, Tick)
      
      val time3 = scala.compat.Platform.currentTime
      guiInterPerf send { _.end(time3) }
      betweenTickPerf send { _.start(time3) }
      
      stay      
    }
    
    case Event(Stop, _) => {
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
 * the NetLogoHeadlessActor needs an id to share data
 * with an instance of the NetLogoHeadlessWorkspace (via the extension).
 * 
 * This kind of sharing data has better performance than setting the parameters 
 * directly and recompiling the reward-reporter 
 * (see also: https://groups.google.com/forum/#!msg/netlogo-devel/8oDmCRERDlQ/0IDZm015eNwJ). 
 */

class NetLogoHeadlessActor(val id: Int) extends Actor {
  import NetLogoActors._
  import QLSystem._
  import org.nlogo.headless.HeadlessWorkspace

  val workspace = HeadlessWorkspace.newInstance
  val rewardRepName = config.getString(cfgstr + ".reward-reporter-name")
  val setupComName = config.getString(cfgstr + ".setup-command-name")
  
  override def postStop() {
    workspace.dispose()
  }
  
  // not ready to handle any requests (list of GroupChoices)
  def idle: Receive = {
    
    case CompileReporter => {
      workspace.modelOpened = false
      workspace.open(org.nlogo.app.App.app.workspace.getModelPath())
      workspace.command(setupComName)
      context.become(ready(workspace.compileReporter(rewardRepName + " " + id), Queue[List[NLGroupChoice]]()) orElse idle)
    }

  }
  
  // waiting to load a model and compile a reporter
  def ready(reporter: org.nlogo.nvm.Procedure, data: Queue[List[NLGroupChoice]]) : Receive = {
    
    case NLGroupChoicesList(list) => {
      Future {
          workspace.runCompiledReporter(workspace.defaultOwner, reporter).asInstanceOf[org.nlogo.api.LogoList]
      } onSuccess {
        case result =>
//          println("HLresult: " + result)
          result.foreach(ar => {
            val groupChoice = ar.asInstanceOf[NLGroupChoice]
            (groupChoice.qlAgents, groupChoice.choices, groupChoice.rewards).zipped.foreach((agent, alt, r) => agent send {_.updated(alt, r)})    
          })
      }
      context.become(ready(reporter, data.enqueue(list)) orElse idle)
    }
    
    case GetNLGroupChoices(_) => {
      val (elements, newData) = data.dequeue
      sender ! NLGroupChoicesList(elements)
      context.become(ready(reporter, newData) orElse idle)
    }
    
  }
  
  def receive = idle

}

/**
 * A custom router that is similar to RoundRobin but also forwards messages with id
 * (GetNLGroupChoices(id)) to a particular NLHeadlessActor
 * 
 */
case class NetLogoHeadlessRouter(size: Int) extends RouterConfig {
 
  def routerDispatcher: String = Dispatchers.DefaultDispatcherId
  def supervisorStrategy: SupervisorStrategy = SupervisorStrategy.defaultStrategy
 
  def createRoute(routeeProps: Props, routeeProvider: RouteeProvider): Route = {
    
    val currentRoutees = if (size < 1) 
      IndexedSeq(routeeProvider.context.system.deadLetters)
    else
      (0 until size).map(id => {
        routeeProvider.context.actorOf(Props(new NetLogoHeadlessActor(id)))
      }).toIndexedSeq
 
    routeeProvider.registerRoutees(currentRoutees)
    
    val next = new AtomicInteger(0)

    def getNext(): ActorRef = { if (size <= 1)
        currentRoutees(0)
      else {
        val nextId = next.get
        next.set((nextId + 1) % size)
        currentRoutees(nextId)
      }
    }

    
    {
      case (sender, message) =>
        message match {
          case NetLogoActors.GetNLGroupChoices(id) =>  List(Destination(sender, currentRoutees(id)))
          case Broadcast(msg) => toAll(sender, currentRoutees)
          case msg            => List(Destination(sender, getNext()))
        }
    }

}
 
}