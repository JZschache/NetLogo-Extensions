package de.qlearning

import de.util.RandomHelper
import de.qlextension.QLExtension

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
  class QValue(val alt: String, val n: Double, val value: Double, val updateFunction: (Double, Double) => Unit) extends HasValue {
    def updated(amount: Double) = { 
      val newValue = value + (1.0 /(n + 1.0)) * (amount - value)
      updateFunction(newValue, n + 1.0)
      new QValue(alt, n + 1.0, newValue, updateFunction)
    }
  }

  /**
   * convenient constructor of the QLAgent
   */
  def apply(exploration: String, experimenting: Double, nlAgent: org.nlogo.agent.Agent) = 
//    new QLAgent(experimenting, Map[String,QValue](), 0.0, "", (exploration match {
    new QLAgent(experimenting, Map[String,QValue](), "", (exploration match {
        case "epsilon-greedy" => epsGreedy
        case "softmax" => softmax
//        case "uncertainty" => uncertainty
      }), 1.0, nlAgent, findExpUpdateFunction(nlAgent))   

  
  ////////////////////////////////////////
  // various decision making algorithms //
  ////////////////////////////////////////
  
  private val epsGreedy = (qValues: List[QValue], epsilon: Double, rh: RandomHelper) => {
    if (rh.uniform.nextDoubleFromTo(0, 1) < epsilon)
      rh.randomComponent(qValues).alt
    else {
      val maxima = maximum(qValues)
      if (maxima.length == 1) maxima.head.alt  else rh.randomComponent(maxima).alt
    }
  }
  private val softmax = (qValues: List[QValue], temperature:Double, rh: RandomHelper) => {
    val t = Math.max(temperature, 0.02) // there is some problem if temperature <= 0.02
    val expForm = qValues.scanLeft(("".asInstanceOf[String], 0.0))((temp, qva) => (qva.alt, temp._2 + scala.math.exp(qva.value / t))).tail
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
  
  val qvaluePrefix = QLSystem.config.getString(QLExtension.cfgstr + ".qvalue-prefix")
  val nPrefix = QLSystem.config.getString(QLExtension.cfgstr + ".n-prefix")
  
  def findUpdateFunction(nlAgent: org.nlogo.agent.Agent, alt: String) = {
    
    val vLength = nlAgent.variables.size
    
    val idxQv = (0 until vLength).toList.find(i => nlAgent.variableName(i) == (qvaluePrefix + alt).toUpperCase())
    val idxN = (0 until vLength).toList.find(i => nlAgent.variableName(i) == (nPrefix + alt).toUpperCase())
    
    (value: Double, n: Double) => {
      if (idxQv.isDefined) 
        nlAgent.setVariable(idxQv.get, value)
      if (idxN.isDefined)
        nlAgent.setVariable(idxN.get, n)
    }
  }
  
  val expRateName = QLSystem.config.getString(QLExtension.cfgstr + ".exploration-rate-name")
  
  def findExpUpdateFunction(nlAgent: org.nlogo.agent.Agent) = {
    
    val vLength = nlAgent.variables.size
    
    val idx = (0 until vLength).toList.find(i => nlAgent.variableName(i) ==  expRateName.toUpperCase())
    
    (value: Double) => {
      if (idx.isDefined) 
        nlAgent.setVariable(idx.get, value)
    }
  }
    
  
}

/**
 *  a QLAgent holds information about the temporary state of an Agent
 *  
 *  it is immutable
 *  
 *  it is used to update the Q-values and to get a decision from the agent
 */ 
case class QLAgent(experimenting: Double, qValuesMap: Map[String,QLAgent.QValue], 
                   lastChoice: String,
                   choiceAlg: (List[QLAgent.QValue], Double, RandomHelper) => String,
                   expDecay: Double, nlAgent: org.nlogo.agent.Agent, expUpdate: Double => Unit) {
  
  def updated(alt:String, reward: Double) : QLAgent = {
    val newQvalue = qValuesMap.getOrElse(alt, new QLAgent.QValue(alt, 0.0, 0.0, QLAgent.findUpdateFunction(nlAgent, alt))).updated(reward)
    val newExp = experimenting * expDecay
    expUpdate(newExp)
    QLAgent(newExp, qValuesMap.updated(alt, newQvalue), 
            alt, choiceAlg, expDecay, nlAgent, expUpdate)
  }
  
  def setAlternatives(alternatives: List[String]) = QLAgent(experimenting,
      alternatives.map(key => (key -> qValuesMap.getOrElse(key, new QLAgent.QValue(key, 0.0, 0.0, QLAgent.findUpdateFunction(nlAgent, key))))).toMap,
      lastChoice, choiceAlg, expDecay, nlAgent, expUpdate)
  
  def choose(alternatives: List[String]): String = {
    choiceAlg(alternatives.map(alt => qValuesMap.getOrElse(alt, new QLAgent.QValue(alt, 0.0, 0.0, QLAgent.findUpdateFunction(nlAgent, alt)))), experimenting, de.util.ThreadLocalRandomHelper.current)
  }
  
  /**
   * after calling this function, the agent successively lowers the  
   * level of experimenting.
   */
  def startDecreasing(experimentingDecay: Double) : QLAgent = 
    QLAgent(experimenting, qValuesMap, lastChoice, choiceAlg, experimentingDecay, nlAgent, expUpdate)
    
}
