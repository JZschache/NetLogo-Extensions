package de.qlearning

//import scala.compat.Platform
import akka.actor.{Actor, ActorRef, FSM}
import akka.agent.{Agent => AkkaAgent}
import akka.dispatch.Future
import akka.pattern.ask
import akka.util.duration._
import akka.util.Timeout
import de.qlearning.util.RandomHelper
import akka.actor.Cancellable
//import akka.util.Timeout


object QLAgent {

  // finding an object with the highest value
  sealed trait HasValue { val value : Double}
  def maximum[A <: HasValue](list: Iterable[A]) = {
    require(list.size >= 1)
    list.tail.foldLeft(List(list.head))((result,b) => 
      scala.math.signum(result.head.value - b.value) match {
        case -1 => List(b)
        case 1 => result
        case 0 => b :: result
      })
  }
  
  // the QValue-Object
  class QValue(val alt: String, val n: Double, val value: Double) extends HasValue {
    
    def updated(amount: Double) : QValue = {
      val newValue = value + (1.0 /(n + 1.0)) * (amount - value)
      new QValue(alt, n + 1.0, newValue)
    }
  }
  
  
  private val epsGreedy = (qValuesMap: Map[String,QValue], alternatives: List[String], epsilon: Double, rh: RandomHelper) => {
    if (rh.uniform.nextDoubleFromTo(0, 1) < epsilon)
      rh.randomComponent(alternatives)
    else {
      val maxima = maximum(alternatives.map(alt => qValuesMap.getOrElse(alt, new QValue(alt, 0.0, 0.0))))
      if (maxima.length == 1) maxima.head.alt  else rh.randomComponent(maxima).alt
    }
  }
    
  private val softmax = (qValuesMap: Map[String,QValue], alternatives: List[String], temperature:Double, rh: RandomHelper) => {
    val qvalues = alternatives.map(alt => qValuesMap.getOrElse(alt, new QValue(alt, 0.0, 0.0)))
    val expForm = qvalues.scanLeft(("".asInstanceOf[String], 0.0))((temp, qva) => (qva.alt, temp._2 + scala.math.exp(qva.value / temperature))).tail
    val randomValue = rh.uniform.nextDoubleFromTo(0, expForm.last._2)
    expForm.find(randomValue < _._2).get._1    
  }
  
  def apply(exploration: String, experimenting: Double) = 
    new QLAgent(experimenting, Map[String,QValue](), 0.0, "", (exploration match {
        case "epsilon-greedy" => epsGreedy
        case "softmax" => softmax
      }), false, 1.0)
  
  // various decision making algorithms
//  def getDecisionAlgorithm(exploration: String, dataAgent: AkkaAgent[QLAgent.QLData]) = {
//    exploration match {
//      case "epsilon-greedy" => epsGreedy(dataAgent, _:List[String], _:Double, _:RandomHelper)
//      case "softmax" => softmax(dataAgent, _:List[String], _:Double, _:RandomHelper)
//    }
//  }
  
  
  // a class that holds data and helps with the decision making
//  class Decisions(val experimenting: Double, val decisionAlg: (List[String], Double, RandomHelper) => String, var decrease: Boolean = false, var n: Double = 0.0) {
//
//    def next(alternatives: List[String], rh:RandomHelper) = {
//      if (decrease){
//        n += 1.0
//        decisionAlg(alternatives, experimenting / n, rh)
//      } else
//        decisionAlg(alternatives, experimenting, rh)
//    }
//    
//    def startDecreasing = {
//      decrease = true
//    }
//    
//  }

  // messages QLAgent
//  case object DecExp
//  case class Choose(alternatives: List[String], rh: RandomHelper)
//  case class Choice(alternative: String)
//  case class Reward(alternative: String, amount: Double)

  // states GroupHandler
  sealed class State
  case object Idle extends State
  case object Active extends State
  // data GroupHandler
  sealed class Data
  case object NoGroups extends Data
  case object WithGroups extends Data
  case class WithScheduler(scheduler: Cancellable) extends Data
  //messages GroupHandler
//  case class SetGroup(nlGroup: NLGroup)
  case object Tick
//  case class Start(pauseMS: Int)
//  case object Stop
  case class HandleGroups(groups: List[NLGroup])
    
}

// an object holding all the data of interest
//  class QLData(val qValuesMap: Map[String,QValue], val nMap: Map[String, Double], val nTotal: Double, val lastChoice: String, val decrease: Boolean, val decN: Double) {
class QLAgent(val experimenting: Double, val qValuesMap: Map[String,QLAgent.QValue], val nTotal: Double, val lastChoice: String, 
  val choiceAlg: (Map[String,QLAgent.QValue], List[String], Double, RandomHelper) => String,
  val decrease: Boolean, val decN: Double) {
  
//    def ++(newAlternatives: List[String]) = {
//      val newQvalue = qValuesMap ++ newAlternatives.map(alt => (alt -> new QValue(alt, 0.0, 0.0)))
//      val newN = nMap ++ newAlternatives.map(_ -> 0.0)
//      new QLData(newQvalue, newN, nTotal, lastChoice)
//    }
    
    
    def updated(alt:String, reward: Double) : QLAgent = {
      val newQvalue = qValuesMap.getOrElse(alt, new QLAgent.QValue(alt, 0.0, 0.0)).updated(reward)
//      val newN = nMap.getOrElse(alt, 0.0) + 1.0
//      new QLData(qValuesMap.updated(alt, newQvalue), nMap.updated(alt, newN), nTotal + 1.0, alt, decrease, (if (decrease) decN + 1.0 else decN))
      new QLAgent(experimenting, qValuesMap.updated(alt, newQvalue), nTotal + 1.0, alt, choiceAlg, decrease, (if (decrease) decN + 1.0 else decN))
    }
    
//    def this() = this(Map[String,QValue](), Map[String, Double](), 0.0, "", false, 1.0)
   
    
//    def this(newAlternatives: List[String]) = this(newAlternatives.map(alt => (alt -> new QValue(alt, 0.0, 0.0))).toMap,
//        newAlternatives.map(_ -> 0.0).toMap, 0.0, "")
    
    def choose(alternatives: List[String], rh:RandomHelper): String = 
      choiceAlg(qValuesMap, alternatives, experimenting / decN, rh)
    
    def startDecreasing : QLAgent = 
      new QLAgent(experimenting, qValuesMap, nTotal, lastChoice, choiceAlg, true, decN)
    
  }

class GroupHandler(router: ActorRef, seed:Int, fixedGroups: List[NLGroup]) extends Actor with FSM[QLAgent.State, QLAgent.Data]{
  import QLAgent._
  
  val rh = new util.RandomHelper(seed)
//  implicit val ec = context.dispatcher
  
  
//  private def handleGroup(group: NLGroup) = {
//    implicit val timeout = Timeout(60 seconds)
//    Future.sequence(group.group.map(triple => {
//      val ar = triple._2
//      val future = (ar ? Choose(triple._3, rh)).mapTo[Choice]
//      future.map(f => (triple._1, ar, f))
//    })) onSuccess {
//      case result =>
//        val unzipped = result.unzip3
//        router ! NetLogoActors.GroupChoice(unzipped._1, unzipped._2, unzipped._3.map(_.alternative))
//    }
//  } 
  
  private def handleGroup(group: NLGroup) = {
    val result = group.alternatives.map(pair => {
      // choice may be done before all updates (QValue) have been placed
      (pair._1, pair._1.get.choose(pair._2, rh))
    })
    val unzipped = result.unzip
    router ! NetLogoActors.GroupChoice(group.nlAgents, unzipped._1, unzipped._2)
  }
    
  if (fixedGroups.isEmpty)
    startWith(Active, NoGroups)
  else
    startWith(Active, WithGroups)
    
//  when(Idle) {
//    case Event(Start(pauseMS), WithGroups) => {
//      println("Start WithGroups")
//      val scheduler = context.system.scheduler.schedule(pauseMS.milliseconds, pauseMS.milliseconds, self, Tick)
//      goto(Active) using WithScheduler(scheduler)
//    }
//  }
  
  when(Active) {
    case Event(Tick, _) => {
//      println("Tick in GroupActor")
      fixedGroups.foreach(handleGroup(_))
      stay
    }
//    case Event(Stop, WithScheduler(scheduler)) => {
//      scheduler.cancel
//      goto(Idle) using WithGroups
//    }
  }
  
  whenUnhandled {
    case Event(HandleGroups(grouplist: List[NLGroup]), _) => {
      grouplist.foreach(handleGroup(_))
      stay
    }
//    case Event(Tick, _) =>
//      stay
//    case Event(Stop, _) =>
//      stay
//    case Event(Start, _) =>
//      stay
  }
      
  initialize
  
}

//class QLAgent(val dataAgent: AkkaAgent[QLAgent.QLData], val experimenting: Double, val exploration: String) extends Actor {
//  import QLSystem._
//  import QLAgent._
//  
//  
////  val generator: RandomEngine  = new MersenneTwister64(Platform.currentTime.toInt)
////  val uniform = new Uniform(generator)
//  
//  private var decisions = new Decisions(experimenting, getDecisionAlgorithm(exploration, dataAgent))
//  
////  startWith(Idle, Uninitialized)
//  
////  when(Idle) {
////    case Event(Init(experimenting, exploration), _) =>
////      dataAgent update new QLData()
////      stay using Initialized(new Decision(experimenting, exploration))
//      
//  def receive = {
////    case AddChoiceAltList(altList, replace) =>
////      if (replace)
////        dataAgent update new QLData(altList)
////      else
////        dataAgent send { _ ++ altList }
//      
////    case Event(AddGroup(newGroup: ActorRef), Initialized(groups, lastChoices, choice)) =>
////      
////      stay using Initialized(newGroup :: groups, lastChoices, choice)
////      
//      
////    case Event(Start, data: Initialized) =>
////      goto(Choosing)
////  }
//  
////  onTransition {
////    case _ -> Choosing =>
////      context.system.scheduler.scheduleOnce(QLSystem.pbc.milliseconds, self, Choose)
////  }
//  
////  when(Choosing){
//    
//    case Choose(altList, rh) => 
////      println("Choose")
//      sender ! Choice(decisions.next(altList, rh))
////      val decisions = if (groups.isEmpty){
////        val c = choice.next
////        environment ! Choice(c)
////        List[String](c)
////      } else {
////        // what to do if member of multiple groups?
////        val c = choice.next
////        groups.first ! Choice(c)
////        List[String](c)
////      }
////      goto(Waiting) using Initialized(choice.update)
//      
//    case Reward(alt, amount) =>
//      dataAgent send { _.updated(alt, amount) }
//      
//    case DecExp => 
//      decisions.startDecreasing
//  }
//  
////  when(Waiting){
////    case Event(Reward(alt, amount), _) =>
////      dataAgent send { _.updated(alt, amount) }
////      // what to do if member of multiple groups ?
////      goto(Choosing)
////  }
//  
////  whenUnhandled {
////    case Event(Stop, _) =>
////      goto(Idle)
////    case Event(Reward(_, _), _) =>
////      stay
////    case Event(Choose,_) =>
////      stay
////    case Event(DecExp, Initialized(choice)) =>
////      stay using Initialized(choice.startDecreasing)
////  }
////    
////  initialize
//}