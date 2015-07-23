package de.qlearning

import de.util.RandomHelper

object QLAgent {

  sealed trait HasValue { val value : Double}
  /**
   * finding all HasValue-objects with the highest value
   */
  def maximum[A <: HasValue](list: Iterable[A]) = {
    require(list.size >= 1)
    list.tail.foldLeft(List(list.head))((result,b) => 
      scala.math.signum(result.head.value - b.value) match {
        case -1 => List(b)
        case 1 => result
        case 0 => b :: result
      })
  }
  
  /**
   *  the QValue (immutable)
   */ 
  class QValue(val alt: String, val n: Double, val value: Double) extends HasValue {
    def updated(amount: Double) = 
      new QValue(alt, n + 1.0, (value + (1.0 /(n + 1.0)) * (amount - value)))
  }

  /**
   * convenient constructor of the QLAgent
   */
  def apply(exploration: String, experimenting: Double) = 
    new QLAgent(experimenting, Map[String,QValue](), 0.0, "", (exploration match {
        case "epsilon-greedy" => epsGreedy
        case "softmax" => softmax
//        case "uncertainty" => uncertainty
      }), 1.0)   

  
  ////////////////////////////////////////
  // various decision making algorithms //
  ////////////////////////////////////////
  
  private val epsGreedy = (qValuesMap: Map[String,QValue], alternatives: List[String], epsilon: Double, rh: RandomHelper) => {
    if (rh.uniform.nextDoubleFromTo(0, 1) < epsilon)
      rh.randomComponent(alternatives)
    else {
      val maxima = maximum(alternatives.map(alt => qValuesMap.getOrElse(alt, new QValue(alt, 0.0, 0.0))))
      if (maxima.length == 1) maxima.head.alt  else rh.randomComponent(maxima).alt
    }
  }
  private val softmax = (qValuesMap: Map[String,QValue], alternatives: List[String], temperature:Double, rh: RandomHelper) => {
    val t = Math.max(temperature, 0.02) // there is some problem if temperature <= 0.02
    val qvalues = alternatives.map(alt => qValuesMap.getOrElse(alt, new QValue(alt, 0.0, 0.0)))
    val expForm = qvalues.scanLeft(("".asInstanceOf[String], 0.0))((temp, qva) => (qva.alt, temp._2 + scala.math.exp(qva.value / t))).tail
    val randomValue = rh.uniform.nextDoubleFromTo(0, expForm.last._2)
    expForm.find(randomValue < _._2).get._1
  }

//  private val uncertainty = (qValuesMap: Map[String,QValue], alternatives: List[String], epsilon: Double, rh: RandomHelper) => {
//    if (rh.uniform.nextDoubleFromTo(0, 1) < epsilon) {
//      val qvalues = alternatives.map(alt => qValuesMap.getOrElse(alt, new QValue(alt, 0.0, 0.0)))
//      val ns = qvalues.scanLeft(("".asInstanceOf[String], 0.0))((temp, qva) => (qva.alt, temp._2 + ( if (qva.n > 0.0) 1.0 / qva.n else 1.0))).tail
//      val randomValue = rh.uniform.nextDoubleFromTo(0, ns.last._2)
//      ns.find(randomValue < _._2).get._1
//    } else {
//      val maxima = maximum(alternatives.map(alt => qValuesMap.getOrElse(alt, new QValue(alt, 0.0, 0.0))))
//      if (maxima.length == 1) maxima.head.alt  else rh.randomComponent(maxima).alt
//    }
//  }
  
}

/**
 *  a QLAgent holds information about the temporary state of an Agent
 *  
 *  it is immutable
 *  
 *  it is used to update the Q-values and to get a decision from the agent
 */ 
case class QLAgent(experimenting: Double, qValuesMap: Map[String,QLAgent.QValue], 
                   nTotal: Double, lastChoice: String, 
                   choiceAlg: (Map[String,QLAgent.QValue], List[String], Double, RandomHelper) => String,
                   expDecay: Double) {
  
  def updated(alt:String, reward: Double) : QLAgent = {
//    println("update: " + alt + " " + reward)
    val newQvalue = qValuesMap.getOrElse(alt, new QLAgent.QValue(alt, 0.0, 0.0)).updated(reward)
    QLAgent(Math.max(experimenting * expDecay, 0.02), qValuesMap.updated(alt, newQvalue), 
            nTotal + 1.0, alt, choiceAlg, expDecay)
  }
  
  def setAlternatives(alternatives: List[String]) = QLAgent(experimenting,
      alternatives.map(key => (key -> qValuesMap.getOrElse(key, new QLAgent.QValue(key, 0.0, 0.0)))).toMap,
      nTotal, lastChoice, choiceAlg, expDecay)
  
  def choose(alternatives: List[String]): String = {
    choiceAlg(qValuesMap, alternatives, experimenting, de.util.ThreadLocalRandomHelper.current)
  }
  
  /**
   * after calling this function, the agent successively lowers the  
   * level of experimenting.
   */
  def startDecreasing(experimentingDecay: Double) : QLAgent = 
    QLAgent(experimenting, qValuesMap, nTotal, lastChoice, choiceAlg, experimentingDecay)
    
}
