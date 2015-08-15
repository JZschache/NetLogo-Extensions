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
import de.qlextension.QLExtension


object NetLogoSupervisor {
  
  // states
  sealed trait State
  case object Idle extends State
  case object Supervising extends State
  // data
  sealed trait Data
  case object Uninitialized extends Data
  sealed trait InitializedTrait extends Data {
    def remainingBatches: List[List[NLGroup]]
    def headlessIds: List[Int]
    def updateCommand: org.nlogo.nvm.Procedure
    def tickCount : Int
    def copy(remainingBatches: List[List[NLGroup]], headlessIds: List[Int]): InitializedTrait
    def copy(tickCount: Int): InitializedTrait
  }
  case class Initialized(override val remainingBatches: List[List[NLGroup]], override val  headlessIds: List[Int], override val updateCommand: org.nlogo.nvm.Procedure, override val tickCount : Int) extends InitializedTrait {
    override def copy(batches: List[List[NLGroup]], ids: List[Int]) = Initialized(batches, ids, updateCommand, tickCount)
    override def copy(count: Int) = Initialized(remainingBatches, headlessIds, updateCommand, count)
  }
  case class WithGroupStructure(groups: List[List[NLGroup]], override val remainingBatches: List[List[NLGroup]], override val  headlessIds: List[Int], override val updateCommand: org.nlogo.nvm.Procedure, override val tickCount : Int) extends InitializedTrait {
    override def copy(batches: List[List[NLGroup]], ids: List[Int]) = WithGroupStructure(groups, batches, ids, updateCommand, tickCount)
    override def copy(count: Int) = WithGroupStructure(groups, remainingBatches, headlessIds, updateCommand, count)
  }
  case class WithGroupReporter(groupReporter: org.nlogo.nvm.Procedure, override val remainingBatches: List[List[NLGroup]], override val  headlessIds: List[Int], override val updateCommand: org.nlogo.nvm.Procedure, override val tickCount : Int) extends InitializedTrait {
    override def copy(batches: List[List[NLGroup]], ids: List[Int]) = WithGroupReporter(groupReporter, batches, ids, updateCommand, tickCount)
    override def copy(count: Int) = WithGroupReporter(groupReporter, remainingBatches, headlessIds, updateCommand, count)
  }
  //messages
  case object InitNetLogoActors
  case class SetGroupStructure(structure: List[NLGroup])
  case object Start
  case object Stop
  case class NLGroupsList(headlessId: Int, groups: List[NLGroup])
   
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
class NetLogoSupervisor(netLogoRouter: ActorRef) extends Actor with FSM[NetLogoSupervisor.State, NetLogoSupervisor.Data]{
  import NetLogoSupervisor._
  import QLSystem._
  
  val nlApp = org.nlogo.app.App.app
    
  val groupRepName = config.getString(QLExtension.cfgstr + ".group-reporter-name")
  val updateComName = config.getString(QLExtension.cfgstr + ".update-command-name")
  
  //private message
  case object Tick
  case object UpdateNetLogo
  
  //private functions
  private def cutToBatches(list: List[NLGroup]) = {
    if (conHeadlessEnv > 1) {
      // one batch for each HeadlessActor
      val batchSize = Math.ceil(list.size.toDouble / conHeadlessEnv.toDouble).toInt
      (1 to conHeadlessEnv).foldLeft((list, List[List[NLGroup]]())) { (pair, i) =>
        val (front, tail) = pair._1.splitAt(batchSize)  
        (tail, front :: pair._2)
      }._2
    } else List(list)
  }
  private def tick = {
    //speedSliderPosition is a number between -110 (very slow) and 110 (very fast)
    val guiSpeed = (nlApp.workspace.speedSliderPosition() - 110.0) * (-1.0)
    if (guiSpeed == 0.0)
      self ! Tick
    else
      context.system.scheduler.scheduleOnce(guiSpeed.milliseconds, self, Tick)
  }
  
  startWith(Idle, Uninitialized)
  
  when(Idle) {
    case Event(InitNetLogoActors, _) => {
      // save changes
      val ms = new ModelSaver(nlApp)
      val path = nlApp.workspace.getModelPath()
      org.nlogo.api.FileIO.writeFile(path, ms.save)
      // the NetLogo-model is reloaded and the reward-reporter is recompiled
      netLogoRouter ! Broadcast(NetLogoHeadlessActor.OpenModel(path))
      // recompile the update command
      val updateCommand = nlApp.workspace.compileCommands(updateComName)
      // the groups structure is deleted
      stay using Initialized(Nil, Nil, updateCommand, 0)
    }
      
    // SetGroupStructure message only works if Initialized
    case Event(SetGroupStructure(structure: List[NLGroup]), Initialized(remainingBatches, headlessIds, updateCommand, tickCount)) => {
      stay using WithGroupStructure(cutToBatches(structure), remainingBatches, headlessIds, updateCommand, tickCount)
    }
    
    // start without fixed GroupStructure
    // the group-reporter is called repeatedly in order to get a list NLGroups 
    case Event(Start, Initialized(remainingBatches, headlessIds, updateCommand, tickCount)) => {
      val groupReporter = nlApp.workspace.compileReporter(groupRepName)
      tickPerf send { _.start(scala.compat.Platform.currentTime)}
      goto(Supervising) using WithGroupReporter(groupReporter, remainingBatches, headlessIds, updateCommand, tickCount)
    }
    
    // start with fixed GroupStructure
    case Event(Start, WithGroupStructure(_,_,_,_,_)) => {
      tickPerf send { _.start(scala.compat.Platform.currentTime)}
      goto(Supervising)
    }
    
    case Event(Tick, _) => // received when HeadlessActors become ready
      stay
    case Event(UpdateNetLogo, _) => // received after stopping
      stay
  }
  
  onTransition {
    case Idle -> Supervising =>
      stateData match {
        case d: InitializedTrait => {
          if (!d.headlessIds.isEmpty) tick 
        }
        case _ => // to nothing
      }
      
  }
  
  // in this state: the NetLogoSupervisor uses the batchStructure or the groupReporter
  // to repeatedly call the handleGroups-function
  when(Supervising) {
    
    case Event(Tick, d: InitializedTrait) => {
      
      val time1 = scala.compat.Platform.currentTime
      QLSystem.betweenTickPerf send { _.end(time1) }
      QLSystem.handleGroupPerf send { _.start(time1)}
            
      val batches = if (d.remainingBatches.isEmpty) {
        self ! UpdateNetLogo
        d match {
          case WithGroupStructure(groups, remainingBatches, headlessIds,_,_) => groups
          case WithGroupReporter(groupReporter,remainingBatches, headlessIds,_,_) => {
            val logolist = nlApp.workspace.runCompiledReporter(nlApp.owner, groupReporter).asInstanceOf[org.nlogo.api.LogoList]
            cutToBatches(logolist.map(_.asInstanceOf[NLGroup]).toList)
          }
          case Initialized(_,_,_,_) => // should not happen 
            Nil
        }
      } else {
        d.remainingBatches
      }
           
      netLogoRouter ! NLGroupsList(d.headlessIds.first, batches.first)
      
      if (!d.headlessIds.tail.isEmpty) tick 
      
      val time2 = scala.compat.Platform.currentTime
      QLSystem.handleGroupPerf send { _.end(time2)}
      QLSystem.betweenTickPerf send { _.start(time2) }
      
      stay using d.copy(batches.tail, d.headlessIds.tail)
    }
    
    case Event(UpdateNetLogo, d: InitializedTrait) => {
      
      val time1 = scala.compat.Platform.currentTime
      QLSystem.betweenTickPerf send { _.end(time1) }
      QLSystem.guiInterPerf send { _.start(time1) }
      
      nlApp.workspace.runCompiledCommands(nlApp.owner, d.updateCommand)
      
      val nextTick = if (d.tickCount == 99) {
        val time = scala.compat.Platform.currentTime
        tickPerf send { _.end(time)}
        tickPerf send { _.start(time)}
        0
      } else {
        d.tickCount + 1
      }
      
      val time2 = scala.compat.Platform.currentTime
      QLSystem.guiInterPerf send { _.end(time2) }
      QLSystem.betweenTickPerf send { _.start(time2) }
      
      stay using d.copy(nextTick)
    }
    
    case Event(Stop, _) => {
      goto(Idle)
    }
    
  }
  
  whenUnhandled {
    case Event(NetLogoHeadlessActor.IAmReady(id), d: InitializedTrait) => {
      if (d.headlessIds.isEmpty) // starting the Ticks again
        tick
      stay using d.copy(d.remainingBatches, id :: d.headlessIds)
    }
    
  }
  
  initialize
  
}