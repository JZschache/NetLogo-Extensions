package de.qlearning

import scala.compat.Platform

import akka.actor.{Actor, ActorRef, FSM}

import cern.jet.random.engine.MersenneTwister64
import cern.jet.random.engine.RandomEngine
import cern.jet.random.Uniform

import org.nlogo.app.App

import de.qlearning.util.RandomHelper


object QLAgent {

  sealed trait HasValue { val value : Double}
  
  class QValue(val alt: String, n: Int, val value: Double) extends HasValue {
    
    def updated(amount: Double) : QValue = {
      val newValue = value + (1/(n + 1)) * (amount - value)
      new QValue(alt, n + 1, newValue)
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
  case class Data(environment: ActorRef, qValuesMap:Map[String,QValue], totalN: Int, lastChoice: Option[String]) extends DataTrait
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

class EnvironmentActor(val reporterName: String) extends Actor {
  import QLAgent._
  
  def receive = {
    case Choice(agent, alternative) => {
      val result = if (agent.isInstanceOf[org.nlogo.api.Turtle])
        App.app.report(reporterName + " " + agent.getVariable(0).asInstanceOf[Double].round 
            + " \"" + alternative + "\"").asInstanceOf[Double]
      else
        App.app.report(reporterName + " " + agent.getVariable(0).asInstanceOf[Double].round 
            + " " + agent.getVariable(1).asInstanceOf[Double].round
            + " \"" + alternative + "\"").asInstanceOf[Double]
      sender ! Reward(result)
    }
  }
}

class QLAgent(val agent: org.nlogo.api.Agent, val experimenting: Double) extends Actor with FSM[QLAgent.AgentState, QLAgent.DataTrait]{
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
      stay using Data(environment, Map[String,QValue](), 0, None)
      
    case Event(AddChoiceAltList(altList, replace), Data(environment, currentMap, totalN, lastChoice)) =>
      val oldMap = replace match {
        case true => Map[String, QValue]()
        case false => currentMap
      }
      stay using Data(environment, oldMap ++ altList.map(alt => (alt -> new QValue(alt,0, 0.0))), totalN, lastChoice)
      
    case Event(Start, _) =>
      goto(Choosing)
  }
  
  onTransition {
    case _ -> Choosing =>
      self ! Choose      
  }
  
  when(Choosing){
    case Event(Choose, Data(environment, currentMap, totalN, lastChoice)) => 
      
      val choice = if (uniform.nextDoubleFromTo(0, 1) < experimenting) {
        RandomHelper.randomComponent(currentMap.values).alt
      } else {
        maximum(currentMap.values).alt
      }
      
      environment ! Choice(this.agent, choice)
      goto(Waiting) using Data(environment, currentMap, totalN + 1, Some(choice))
  }
  
  when(Waiting){
    case Event(Reward(amount), Data(environment, currentMap, totalN, lastChoice)) =>
 
      val newMap = if (lastChoice.isDefined) {
        val alt = lastChoice.get
        currentMap.updated(alt, currentMap(alt).updated(amount))
      } else {
        currentMap
      }
    
      Thread.sleep(10)
      goto(Choosing) using Data(environment, newMap, totalN, None)
  }
  
  whenUnhandled {
    case Event(Stop, _) =>
      goto(Idle)
  }
    
  initialize
}