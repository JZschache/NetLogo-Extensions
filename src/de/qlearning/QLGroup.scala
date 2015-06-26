package de.qlearning

import akka.actor.{Actor, ActorRef, FSM}


object QLGroup {
  
  case class Choices(group: ActorRef, alternatives: List[String])
  case class Rewards(amounts: List[Double])
  
  // states
  sealed trait GroupState
  case object Idle extends GroupState
  case object Waiting extends GroupState
  // data
  sealed trait GroupData
  case object Uninitialized extends GroupData
  case class Initialized(senders: List[ActorRef], messages: List[String]) extends GroupData
}

class QLGroup(val environment: ActorRef, val groupSize: Int) extends Actor  with FSM[QLGroup.GroupState, QLGroup.GroupData]{
  import QLGroup._
  
  startWith(Idle, Initialized(List[ActorRef](), List[String]()))
  
  when(Idle) {
    case Event(QLAgent.Choice(alternative), Initialized(senders, messages)) =>
      val newSenders = sender :: senders
      val newMessages = alternative :: messages
      if (newSenders.size == groupSize) {
        environment ! Choices(self, newMessages)
        goto(Waiting) using Initialized(newSenders, newMessages)
      } else {
        stay using Initialized(newSenders, newMessages)
      }
  }
  
  when(Waiting){
    case Event(Rewards(amounts), Initialized(senders, messages)) =>
      (senders, messages, amounts).zipped.foreach((agent, alt, amount) => agent ! QLAgent.Reward(alt, amount))
      goto(Idle) using Initialized(List[ActorRef](), List[String]())
  }
    
  initialize
  
}