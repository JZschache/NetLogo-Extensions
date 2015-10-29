package de.qlearning

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.immutable.Queue
import akka.actor.{Actor,ActorRef,FSM,SupervisorStrategy,Props}
import akka.agent.{Agent => AkkaAgent}
import akka.routing.{Broadcast,Route,RouteeProvider,RouterConfig,Destination}
import akka.dispatch.{Future,Dispatchers}
import akka.util.duration._
import org.nlogo.nvm.Workspace
import de.qlextension.QLExtension
import de.util.PerformanceMeasure
import org.nlogo.headless.HeadlessWorkspace
import org.nlogo.app.ModelSaver

object NetLogoSupervisor {
  
  // states
  sealed trait State
  case object Idle extends State
  case object Supervising extends State
  // data
  sealed trait Data
  case object Uninitialized extends Data
  sealed trait InitializedTrait extends Data {
    def workspace: Workspace
    def remainingBatches: List[List[NLGroup]]
    def headlessIds: List[Int]
    def updateCommand: org.nlogo.nvm.Procedure
    def tickCount : Int
    def copy(remainingBatches: List[List[NLGroup]], headlessIds: List[Int]): InitializedTrait
    def copy(tickCount: Int): InitializedTrait
  }
  case class Initialized(override val workspace: Workspace, override val remainingBatches: List[List[NLGroup]], override val  headlessIds: List[Int], override val updateCommand: org.nlogo.nvm.Procedure, override val tickCount : Int) extends InitializedTrait {
    override def copy(batches: List[List[NLGroup]], ids: List[Int]) = Initialized(workspace, batches, ids, updateCommand, tickCount)
    override def copy(count: Int) = Initialized(workspace, remainingBatches, headlessIds, updateCommand, count)
  }
  case class WithGroupStructure(groups: List[List[NLGroup]], override val workspace: Workspace, override val remainingBatches: List[List[NLGroup]], override val  headlessIds: List[Int], override val updateCommand: org.nlogo.nvm.Procedure, override val tickCount : Int) extends InitializedTrait {
    override def copy(batches: List[List[NLGroup]], ids: List[Int]) = WithGroupStructure(groups, workspace, batches, ids, updateCommand, tickCount)
    override def copy(count: Int) = WithGroupStructure(groups, workspace, remainingBatches, headlessIds, updateCommand, count)
  }
  case class WithGroupReporter(groupReporter: org.nlogo.nvm.Procedure, override val workspace: Workspace, override val remainingBatches: List[List[NLGroup]], override val  headlessIds: List[Int], override val updateCommand: org.nlogo.nvm.Procedure, override val tickCount : Int) extends InitializedTrait {
    override def copy(batches: List[List[NLGroup]], ids: List[Int]) = WithGroupReporter(groupReporter, workspace, batches, ids, updateCommand, tickCount)
    override def copy(count: Int) = WithGroupReporter(groupReporter, workspace, remainingBatches, headlessIds, updateCommand, count)
  }
  //messages
  case class InitNetLogoActors(workspace: Workspace)
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
//  import QLSystem._
  
  val nlApp = org.nlogo.app.App.app
    
  val groupRepName = QLSystem.config.getString(QLExtension.cfgstr + ".parallel.group-reporter-name")
  val updateComName = QLSystem.config.getString(QLExtension.cfgstr + ".parallel.update-command-name")
  val conHeadlessEnv = QLSystem.config.getInt(QLExtension.cfgstr + ".parallel.headless-workspaces")
  
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
  private def tick(isHeadless: Boolean) = {
    //speedSliderPosition is a number between -110 (very slow) and 110 (very fast)
    val guiSpeed = if (isHeadless) 0.0 else (nlApp.workspace.speedSliderPosition() - 110.0) * (-1.0)
    if (guiSpeed == 0.0)
      self ! Tick
    else
      context.system.scheduler.scheduleOnce(guiSpeed.milliseconds, self, Tick)
  }
  
  startWith(Idle, Uninitialized)
  
  when(Idle) {
    case Event(InitNetLogoActors(workspace), _) => {
      
      val path = workspace.getModelPath()
      // save changes to the model (if not headless)
      if (!workspace.isHeadless()){
        val ms = new ModelSaver(nlApp)
        org.nlogo.api.FileIO.writeFile(path, ms.save)
      }
      // get global variables
      // see setVariables(settings: List[Pair[String, Any]]) in org.nlogo.lab.Worker
//      val nonObsVars = List(("MIN-PXCOR", workspace.world().minPxcor()), 
//                         ("MAX-PXCOR", workspace.world().maxPxcor()),
//                         ("MIN-PYCOR", workspace.world().maxPycor()),
//                         ("MAX-PYCOR", workspace.world().maxPycor()),
//                         ("WORLD-WIDTH", workspace.world().worldWidth()),
//                         ("WORLD-HEIGHT", workspace.world().worldHeight()),
//                         ("RANDOM-SEED", ??))
      val obs = workspace.world().observer()
      val settings = (0 until obs.variables().length).map(i => (obs.variableName(i), obs.getVariable(i))).toList
      
      // the NetLogo-model is reloaded and the reward-reporter is recompiled
      netLogoRouter ! Broadcast(NetLogoHeadlessActor.OpenModel(path, settings))
      // recompile the update command
      val updateCommand = workspace.compileCommands(updateComName)
      // the groups structure is deleted
      stay using Initialized(workspace, Nil, Nil, updateCommand, 0)
    }
      
    // SetGroupStructure message only works if Initialized
    case Event(SetGroupStructure(structure: List[NLGroup]), Initialized(ws, remainingBatches, headlessIds, updateCommand, tickCount)) => {
      stay using WithGroupStructure(cutToBatches(structure), ws, cutToBatches(structure), headlessIds, updateCommand, tickCount)
    }
    
    // start without fixed GroupStructure
    // the group-reporter is called repeatedly in order to get a list NLGroups 
    case Event(Start, Initialized(workspace, remainingBatches, headlessIds, updateCommand, tickCount)) => {
      val groupReporter = workspace.compileReporter(groupRepName)
      val owner = if (workspace.isHeadless()) workspace.asInstanceOf[HeadlessWorkspace].defaultOwner else nlApp.owner
      val logolist = workspace.runCompiledReporter(owner, groupReporter).asInstanceOf[org.nlogo.api.LogoList]
      val firstBatches = cutToBatches(logolist.map(_.asInstanceOf[NLGroup]).toList)
      QLSystem.perfMeasures.startHundredTicksPerf(scala.compat.Platform.currentTime)
      goto(Supervising) using WithGroupReporter(groupReporter, workspace, firstBatches, headlessIds, updateCommand, tickCount)
    }
    
    // restart without fixed GroupStructure
    // the group-reporter is called repeatedly in order to get a list NLGroups 
    case Event(Start, WithGroupReporter(_,_,_,_,_,_)) => {
      QLSystem.perfMeasures.startHundredTicksPerf(scala.compat.Platform.currentTime)
      goto(Supervising)
    }
    
    // start with fixed GroupStructure
    case Event(Start, WithGroupStructure(_,_,_,_,_,_)) => {
      QLSystem.perfMeasures.startHundredTicksPerf(scala.compat.Platform.currentTime)
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
          if (!d.headlessIds.isEmpty) tick (d.workspace.isHeadless())
        }
        case _ => // to nothing
      }
      
  }
  
  // in this state: the NetLogoSupervisor uses the batchStructure or the groupReporter
  // to repeatedly call the handleGroups-function
  when(Supervising) {
    
    case Event(Tick, d: InitializedTrait) => {
      
      val time1 = scala.compat.Platform.currentTime
      QLSystem.perfMeasures.stopNlSuperIdlePerf(time1)
      QLSystem.perfMeasures.startNlSuperHandleGroupsPerf(time1)
            
      // there should always be headlessIds and batches left
      netLogoRouter ! NLGroupsList(d.headlessIds.first, d.remainingBatches.first)
      
      val batches = if (d.remainingBatches.tail.isEmpty) { // if this batch was the last one
        self ! UpdateNetLogo
        d match {
          case WithGroupStructure(groups, ws, remainingBatches, headlessIds,_,_) => groups
          case WithGroupReporter(groupReporter, workspace,remainingBatches, headlessIds,_,_) => {
            val owner = if (workspace.isHeadless()) workspace.asInstanceOf[HeadlessWorkspace].defaultOwner else nlApp.owner
            val logolist = workspace.runCompiledReporter(owner, groupReporter).asInstanceOf[org.nlogo.api.LogoList]
            cutToBatches(logolist.map(_.asInstanceOf[NLGroup]).toList)
          }
          case Initialized(_,_,_,_,_) => // should not happen 
            Nil
        }
      } else {
        d.remainingBatches.tail
      }
      
      if (!d.headlessIds.tail.isEmpty) 
        tick(d.workspace.isHeadless())
      
      val time2 = scala.compat.Platform.currentTime
      QLSystem.perfMeasures.stopNlSuperHandleGroupsPerf(time2)
      QLSystem.perfMeasures.startNlSuperIdlePerf(time2)
      
      stay using d.copy(batches, d.headlessIds.tail)
    }
    
    case Event(UpdateNetLogo, d: InitializedTrait) => {
      
      val time1 = scala.compat.Platform.currentTime
      QLSystem.perfMeasures.stopNlSuperIdlePerf(time1)
      QLSystem.perfMeasures.startNlSuperUpdatePerf(time1)
            
      val owner = if (d.workspace.isHeadless()) d.workspace.asInstanceOf[HeadlessWorkspace].defaultOwner else nlApp.owner
      d.workspace.runCompiledCommands(owner, d.updateCommand)
      
      val nextTick = if (d.tickCount == 99) {
        val time = scala.compat.Platform.currentTime
        QLSystem.perfMeasures.stopHundredTicksPerf(time)
        QLSystem.perfMeasures.startHundredTicksPerf(time)
        0
      } else {
        d.tickCount + 1
      }
      
      val time2 = scala.compat.Platform.currentTime
      QLSystem.perfMeasures.stopNlSuperUpdatePerf(time2)
      QLSystem.perfMeasures.startNlSuperIdlePerf(time2)
      
      stay using d.copy(nextTick)
    }
    
    case Event(Stop, _) => {
      goto(Idle)
    }
    
  }
  
  whenUnhandled {
    case Event(NetLogoHeadlessActor.IAmReady(id), d: InitializedTrait) => {
      if (d.headlessIds.isEmpty) // starting the Ticks again
        tick(d.workspace.isHeadless())
      stay using d.copy(d.remainingBatches, id :: d.headlessIds)
    }
    
  }
  
  initialize
  
}