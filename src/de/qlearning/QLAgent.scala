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
  
  class QValue(val alt: String, val n: Double, val value: Double) extends HasValue {
    
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
  
  class QLData(val qValuesMap: Map[String,QValue], val nMap: Map[String, Double], val nTotal: Double, val lastChoice: String) {
    
    def ++(newAlternatives: List[String]) = {
      val newQvalue = qValuesMap ++ newAlternatives.map(alt => (alt -> new QValue(alt, 0.0, 0.0)))
      val newN = nMap ++ newAlternatives.map(_ -> 0.0)
      new QLData(newQvalue, newN, nTotal, lastChoice)
    }
    
    def updated(alt:String, reward: Double) : QLData = {
      val newQvalue = qValuesMap.getOrElse(alt, new QValue(alt, 0.0, 0.0)).updated(reward)
      val newN = nMap.getOrElse(alt, 0.0) + 1.0
      new QLData(qValuesMap.updated(alt, newQvalue), nMap.updated(alt, newN), nTotal + 1, alt)
    }
    
    def this() = this(Map[String,QValue](), Map[String, Double](), 0.0, "")
    
    def this(newAlternatives: List[String]) = this(newAlternatives.map(alt => (alt -> new QValue(alt, 0.0, 0.0))).toMap,
        newAlternatives.map(_ -> 0.0).toMap, 0.0, "")
    
  }
        
  // states
  sealed trait AgentState
  case object Idle extends AgentState
  case object Choosing extends AgentState
  case object Waiting extends AgentState
  // data
  sealed trait DataTrait
  case object Uninitialized extends DataTrait
  // messages
  case class Init(environment: ActorRef, experimenting: Double, exploration: String)
  case class AddChoiceAltList(altList: List[String], replace: Boolean)
  case object Start
  case object Stop
  case object DecExp
  
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

class QLAgent(val nlAgent: org.nlogo.api.Agent, val dataAgent: AkkaAgent[QLAgent.QLData]) extends Actor with FSM[QLAgent.AgentState, QLAgent.DataTrait]{
  import QLSystem._
  import QLAgent._
  import FSM._
  
  val generator: RandomEngine  = new MersenneTwister64(Platform.currentTime.toInt)
  val uniform = new Uniform(generator)
     
    private def epsGreedy = (epsilon: Double) => {
    if (uniform.nextDoubleFromTo(0, 1) < epsilon)
      RandomHelper.randomComponent(dataAgent.get.qValuesMap.keys)
    else
      maximum(dataAgent.get.qValuesMap.values).alt
  }
  
  private def softmax = (temperature:Double) => {
    val expForm = dataAgent.get.qValuesMap.values.scanLeft(("".asInstanceOf[String], 0.0))((temp, qva) => (qva.alt, temp._2 + scala.math.exp(qva.value / temperature))).tail
    val randomValue = uniform.nextDoubleFromTo(0, expForm.last._2)
    expForm.find(randomValue < _._2).get._1    
  }
  
  private class Decision(val experimenting: Double, val exploration: String, val decrease: Boolean = false, val n: Double = 1.0) {
    
    val choice = exploration match {
        case "epsilon-greedy" => epsGreedy
        case "softmax" => softmax
    }

    def next() = choice(experimenting / n)
    
    def update ={
      if (decrease)
        new Decision(experimenting, exploration, true, n + 1.0)
      else
        new Decision(experimenting, exploration)
    } 
    
    def startDecreasing = new Decision(experimenting, exploration, true, 1.0)
    
  }
  
  // private messages
  private case object Choose
  private case class Initialized(environment: ActorRef, lastChoice: Option[String], choice: Decision) extends DataTrait
  
  startWith(Idle, Uninitialized)
  
  when(Idle) {
    case Event(Init(environment, experimenting, exploration), _) =>
      dataAgent update new QLData()
      stay using Initialized(environment, None, new Decision(experimenting, exploration))
      
    case Event(AddChoiceAltList(altList, replace), _) =>
      if (replace)
        dataAgent update new QLData(altList)
      else
        dataAgent send { _ ++ altList }
      stay
      
    case Event(Start, data: Initialized) =>
      goto(Choosing)
  }
  
  onTransition {
    case _ -> Choosing =>
      context.system.scheduler.scheduleOnce(QLSystem.pbc.milliseconds, self, Choose)
  }
  
  when(Choosing){
    case Event(Choose, Initialized(environment, lastChoice, choice)) => 
      val c = choice.next
      environment ! Choice(nlAgent, c)
      goto(Waiting) using Initialized(environment, Some(c), choice.update)
  }
  
  when(Waiting){
    case Event(Reward(amount), Initialized(_, lastChoice, _ )) =>
      if (lastChoice.isDefined) {
        val alt = lastChoice.get
        dataAgent send { _.updated(alt, amount) }
      }
      goto(Choosing)
  }
  
  whenUnhandled {
    case Event(Stop, _) =>
      goto(Idle)
    case Event(Reward(_), _) =>
      stay
    case Event(Choose,_) =>
      stay
    case Event(DecExp, Initialized(environment, lastChoice, choice)) =>
      stay using Initialized(environment, lastChoice, choice.startDecreasing)
  }
    
  initialize
}