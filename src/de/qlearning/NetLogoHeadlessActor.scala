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


object NetLogoHeadlessActor {
  
  // states
  sealed trait State
  case object NotReady extends State
  case object Ready extends State
  case object Waiting extends State
  // data
  sealed trait Data
  case object Uninitialized extends Data
  case class Initialized(reporter: org.nlogo.nvm.Procedure, data: List[NLGroupChoice]) extends Data
  //messages
  case class OpenModel(path:String)
  case class NLGroupChoicesList(list: List[NLGroupChoice])
  case class GetNLGroupChoices(id: Int)
  case class IAmReady(headlessId: Int)
}

/**
 * the NetLogoHeadlessActor needs an id to share data
 * with an instance of the NetLogoHeadlessWorkspace (via the extension).
 * 
 * This kind of sharing data has better performance than setting the parameters 
 * directly and recompiling the reward-reporter 
 * (see also: https://groups.google.com/forum/#!msg/netlogo-devel/8oDmCRERDlQ/0IDZm015eNwJ). 
 */

class NetLogoHeadlessActor(val id: Int) extends Actor with FSM[NetLogoHeadlessActor.State, NetLogoHeadlessActor.Data]{
  import NetLogoHeadlessActor._
  import QLSystem._
  import org.nlogo.headless.HeadlessWorkspace

//  val workspace = HeadlessWorkspace.newInstance(classOf[MyHeadlessWorkspace])
  val workspace = HeadlessWorkspace.newInstance
  val rewardRepName = config.getString(cfgstr + ".reward-reporter-name")
  val setupComName = config.getString(cfgstr + ".setup-command-name")
  
  override def postStop() {
    workspace.dispose()
  }
  
  startWith(NotReady, Uninitialized)
  
  when(NotReady) {
    case Event(OpenModel(path), _) => {
      workspace.modelOpened = false
      workspace.open(path)
      try {
        workspace.command(setupComName)
      } catch {
        case  e: org.nlogo.api.CompilerException => {
          System.err.println("Compilation of setup command '" + setupComName + "' failed.")
          e.printStackTrace()
        }
      }
      
      netLogoSuper ! IAmReady(id)
      goto(Ready) using Initialized(workspace.compileReporter(rewardRepName + " " + id), null)
    }
  }
  
  // ready to handle requests (list of NLGroups)
  when(Ready) {
    
    // can only be received in state Ready
    case Event(NetLogoSupervisor.NLGroupsList(_, groups), _) => {
      
      val time1 = scala.compat.Platform.currentTime
      headlessIdlePerf send { m => m.updated(id, m(id).end(time1)) }
      headlessHandleNLGroupPerf send {m => m.updated(id, m(id).start(time1))}
      
      Future.sequence(groups.map(group => Future.sequence((group.qlAgents zip group.alternatives).map(pair => 
        pair._1.future map {_.choose(pair._2)}
      )))) onSuccess {
        case list =>  {
          // forward choices of agents to self
          val groupsChoices = (groups zip list).map(pair => NLGroupChoice(pair._1.nlAgents, pair._1.qlAgents, pair._2, Nil))
          self ! NLGroupChoicesList(groupsChoices)
        }
      }
      
      val time2 = scala.compat.Platform.currentTime
      headlessHandleNLGroupPerf send {m => m.updated(id, m(id).end(time2))}
      headlessIdlePerf send {m => m.updated(id, m(id).start(time2))}
      
      goto(Waiting)
    }

  }
  
  /**
   * first, the actor waits for the choices of the agents (NLGroupChoicesList(groupsChoices))
   * then, the actor waits for the NLHeadlessWorkspace to ask for this data
   * 
   * afterwards, he is ready to receive new requests from NetLogoSupervisor
   */
  when(Waiting) {
    
    case Event(NLGroupChoicesList(list), Initialized(reporter, _)) => {
      
      val time1 = scala.compat.Platform.currentTime
      headlessIdlePerf send {m => m.updated(id, m(id).end(time1))}
      headlessHandleNLGroupChoicePerf send {m => m.updated(id, m(id).start(time1))}
      
      Future {
          workspace.runCompiledReporter(workspace.defaultOwner, reporter).asInstanceOf[org.nlogo.api.LogoList]
      } onSuccess {
        case result =>
          result.foreach(ar => {
            // forwards rewards to agents
            val groupChoice = ar.asInstanceOf[NLGroupChoice]
            (groupChoice.qlAgents, groupChoice.choices, groupChoice.rewards).zipped.foreach((agent, alt, r) => agent send {_.updated(alt, r)})    
          })
      }
      
      val time2 = scala.compat.Platform.currentTime
      headlessHandleNLGroupChoicePerf send {m => m.updated(id, m(id).end(time2))}
      headlessIdlePerf send {m => m.updated(id, m(id).start(time2))}
      
      stay using Initialized(reporter, list)
    }
    
    case Event(GetNLGroupChoices(_), Initialized(_, data)) => {
      
      headlessIdlePerf send {m => m.updated(id, m(id).end(scala.compat.Platform.currentTime))}
            
      sender ! 	NLGroupChoicesList(data)
      
      headlessIdlePerf send {m => m.updated(id, m(id).start(scala.compat.Platform.currentTime))}
      
      netLogoSuper ! IAmReady(id)
      goto(Ready)
    }
    
  }
  
  whenUnhandled {
    // can be received in any state (especially when NotReady)
    case Event(OpenModel(path), _) => {
      workspace.modelOpened = false
      workspace.open(path)
      try {
        workspace.command(setupComName)
      } catch {
        case  e: org.nlogo.api.CompilerException => {
          System.err.println("Compilation of setup command '" + setupComName + "' failed.")
          e.printStackTrace()
        }
      }
      netLogoSuper ! IAmReady(id)
      goto(Ready) using Initialized(workspace.compileReporter(rewardRepName + " " + id), null)
    }
    
//    case Event(NLGroupChoicesList(_), _) => // ignore (e.g. after opening a new model)
//      stay
//      
//    case Event(GetNLGroupChoices(_), _) => // ignore (e.g. after opening a new model)
//      stay
    
  }
  
  initialize

}

/**
 * A custom router that is similar to RoundRobin but also forwards messages with id
 * (GetNLGroupChoices(id)) to a particular NLHeadlessActor
 * 
 */
case class NetLogoHeadlessRouter(size: Int) extends RouterConfig {
 
  def routerDispatcher: String = Dispatchers.DefaultDispatcherId
//  def routerDispatcher: String = "pinned-dispatcher"
  def supervisorStrategy: SupervisorStrategy = SupervisorStrategy.defaultStrategy
 
  def createRoute(routeeProps: Props, routeeProvider: RouteeProvider): Route = {
    
    val currentRoutees = if (size < 1) 
      IndexedSeq(routeeProvider.context.system.deadLetters)
    else
      (0 until size).map(id => {
//        routeeProvider.context.actorOf(Props(new NetLogoHeadlessActor(id)).withDispatcher("pinned-dispatcher"))
        routeeProvider.context.actorOf(Props(new NetLogoHeadlessActor(id)))
      }).toIndexedSeq
 
    routeeProvider.registerRoutees(currentRoutees)
    
//    val next = new AtomicInteger(0)
//
//    def getNext(): ActorRef = { if (size <= 1)
//        currentRoutees(0)
//      else {
//        val nextId = next.get
//        next.set((nextId + 1) % size)
//        currentRoutees(nextId)
//      }
//    }

    
    {
      case (sender, message) =>
        message match {
          case NetLogoHeadlessActor.GetNLGroupChoices(id) => List(Destination(sender, currentRoutees(id)))
          case NetLogoSupervisor.NLGroupsList(id, _) => List(Destination(sender, currentRoutees(id)))
          case Broadcast(msg) => toAll(sender, currentRoutees)
//          case msg => List(Destination(sender, getNext()))
        }
    }

}
 
}