package de.qlearning

import scala.compat.Platform
import akka.actor.{Actor, ActorRef, FSM}
import akka.agent.{Agent => AkkaAgent}
import akka.util.duration._
import cern.jet.random.engine.MersenneTwister64
import cern.jet.random.engine.RandomEngine
import cern.jet.random.Uniform
import org.nlogo.app.App
import de.qlearning.util.RandomHelper
import akka.util.Timeout


object QLAgent {

  sealed trait HasValue { val value : Double}
  
  class QValue(val alt: String, n: Double, val value: Double) extends HasValue {
    
    def updated(amount: Double) : QValue = {
      val newValue = value + (1.0 /(n + 1.0)) * (amount - value)
      new QValue(alt, n + 1.0, newValue)
    }
  }
  
  def maximum[A <: HasValue](list: Iterable[A]) = {
    require(list.size >= 1)
    val maxima = list.tail.foldLeft(List(list.head))((result,b) => 
      scala.math.signum(result.head.value - b.value) match {
        case -1 => List(b)
        case 1 => result
        case 0 => b :: result
      })
    if (maxima.length == 1) maxima.head  else RandomHelper.randomComponent(maxima)
  }
      
  // states
  sealed trait AgentState
  case object Idle extends AgentState
  case object Choosing extends AgentState
  case object Waiting extends AgentState
  // data
  sealed trait DataTrait
  case object Uninitialized extends DataTrait
  case class Data(environment: ActorRef, totalN: Int, lastChoice: Option[String]) extends DataTrait
  // messages
  case class Init(environment: ActorRef)
  case class AddChoiceAltList(altList: List[String], replace: Boolean)
  case class Start
  case object Stop
  case class Choice(agent: org.nlogo.api.Agent, alternative: String)
  case class Reward(amount: Double)

}

/**
 * the reporterTask should take an agent and an alternative and return a reward 
 */

//class EnvironmentActor(val reporterName: String) extends Actor {
//  import QLAgent._ 
//   
//  def receive = {
//    case Choice(agent, alternative) => {
//      val result = if (agent.isInstanceOf[org.nlogo.api.Turtle])
//        App.app.report(reporterName + " " + agent.getVariable(0).asInstanceOf[Double].round 
//            + " \"" + alternative + "\"").asInstanceOf[Double]
//      else
//        App.app.report(reporterName + " " + agent.getVariable(0).asInstanceOf[Double].round 
//            + " " + agent.getVariable(1).asInstanceOf[Double].round
//            + " \"" + alternative + "\"").asInstanceOf[Double]
//      sender ! Reward(result)
//    }
//  }
//}

class QLAgent(val agent: org.nlogo.api.Agent, val experimenting: Double, val qValueAgent: AkkaAgent[Map[String,QLAgent.QValue]], val lastChoiceAgent: AkkaAgent[String]) extends Actor with FSM[QLAgent.AgentState, QLAgent.DataTrait]{
  import QLSystem._
  import QLAgent._
  import FSM._
  
  val generator: RandomEngine  = new MersenneTwister64(Platform.currentTime.toInt)
  val uniform = new Uniform(generator)
  
  // private messages
  private case object Choose
  
  startWith(Idle, Uninitialized)
  when(Idle) {
    case Event(Init(environment), Uninitialized) =>
      qValueAgent update Map[String, QValue]()
      lastChoiceAgent update ""
      stay using Data(environment, 0, None)
      
    case Event(AddChoiceAltList(altList, replace), Data(environment, totalN, lastChoice)) =>
      if (replace)
        qValueAgent update altList.map(alt => (alt -> new QValue(alt, 0.0, 0.0))).toMap
      else
        qValueAgent send { _ ++ altList.map(alt => (alt -> new QValue(alt, 0.0, 0.0))).toMap }

      stay using Data(environment, totalN, lastChoice)
      
    case Event(Start, _) =>
      goto(Choosing)
  }
  
  onTransition {
    case _ -> Choosing =>
      context.system.scheduler.scheduleOnce(100.milliseconds, self, Choose)
  }
  
  when(Choosing){
    case Event(Choose, Data(environment, totalN, lastChoice)) => 
      
      val choice = if (uniform.nextDoubleFromTo(0, 1) < experimenting) {
        RandomHelper.randomComponent(qValueAgent.get.keys)
      } else {
        maximum(qValueAgent.get.values).alt
      }
      implicit val timeout = Timeout(1.second)
      environment ! Choice(this.agent, choice)
      lastChoiceAgent update choice
      goto(Waiting) using Data(environment, totalN + 1, Some(choice))
      
  }
  
  when(Waiting){
    case Event(Reward(amount), Data(environment, totalN, lastChoice)) =>
      if (lastChoice.isDefined) {
        val alt = lastChoice.get
        qValueAgent send {map =>  map.updated(alt, map(alt).updated(amount)) }
      }
      goto(Choosing) using Data(environment, totalN, None)
  }
  
  whenUnhandled {
    case Event(Stop, _) =>
      goto(Idle)
    case Event(Reward(amount), _) =>
      stay
    case Event(Choose,_) =>
      stay
  }
    
  initialize
}