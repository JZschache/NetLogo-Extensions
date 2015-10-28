package de.qlearning

import de.util.RandomHelper
import de.qlextension.QLExtension
import org.nlogo.api.LogoList

object QLAgent {
  import QLSystem._
  
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
  class QValue(val name: String, val freq: Double, val value: Double, val gamma: Double, val nDecay: Double) extends HasValue {
    
    def updated(amount: Double, maxValue: Double, withDecay:Boolean) = { 
      val newValue = value + (1.0 / (freq + 1.0)) * (amount + gamma * maxValue - value)
      new QValue(name, freq + 1.0, newValue, gamma, if (withDecay) nDecay + 1.0 else nDecay)
    }
    
  }

  /**
   * convenient constructor of the QLAgent
   */
  def apply(exploration: String, experimenting: Double, gamma:Double, nlAgent: org.nlogo.agent.Agent) = 
    new QLAgent(experimenting, gamma, Map[String,QValue](), 
        (exploration match {
          case "epsilon-greedy" => epsGreedy
          case "softmax" => softmax
        }), 
        false, findUpdateFunction(nlAgent), 
        (exploration match {
          case "epsilon-greedy" => epsGreedyExperimenting(experimenting, _:Boolean, _:Iterable[QValue])
          case "softmax" => softmaxExperimenting(experimenting, _:Boolean, _:Iterable[QValue])
        }))

  
  ////////////////////////////////////////
  // various decision making algorithms //
  ////////////////////////////////////////
  
  private val epsGreedy = (epsilon: Double, qValues: List[QValue]) => {
    val rh = de.util.ThreadLocalRandomHelper.current
    if (rh.uniform.nextDoubleFromTo(0, 1) < epsilon) {
      rh.randomComponent(qValues).name
    } else {
      val maxima = maximum(qValues)
      if (maxima.length == 1) maxima.head.name else rh.randomComponent(maxima).name
    }
  }
  
  private val epsGreedyExperimenting = (initialEpsilon: Double, expDecay: Boolean, qValues: Iterable[QValue]) => {
    if (expDecay) {
      val totalN = qValues.foldLeft(1.0)((sum, q) => sum + q.nDecay)
      initialEpsilon / Math.log(totalN)
    } else 
      initialEpsilon
  }
  
  private val softmax = (temperature: Double, qValues: List[QValue]) => {
    val rh = de.util.ThreadLocalRandomHelper.current
    val expForm = qValues.scanLeft(("".asInstanceOf[String], 0.0))((result, qva) => (qva.name, result._2 + scala.math.exp(qva.value / temperature))).tail
    val randomValue = rh.uniform.nextDoubleFromTo(0, expForm.last._2)
    expForm.find(randomValue < _._2).get._1
  }
  
  private val softmaxExperimenting = (initialTemp: Double, expDecay: Boolean, qValues: Iterable[QValue]) => {
    if (expDecay) {
      val totalN = qValues.foldLeft(1.0)((sum, q) => sum + q.nDecay)
      Math.max(initialTemp / Math.log(totalN) , 0.02) // there is some problem if temperature <= 0.02
    } else 
      initialTemp
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
    
  def findUpdateFunction(nlAgent: org.nlogo.agent.Agent) = {
    
    val vLength = nlAgent.variables.size
    
    val idxA = (0 until vLength).toList.find(i => nlAgent.variableName(i) == altListName.toUpperCase())
    val idxQ = (0 until vLength).toList.find(i => nlAgent.variableName(i) == qvListName.toUpperCase())
    val idxN = (0 until vLength).toList.find(i => nlAgent.variableName(i) == freqListName.toUpperCase())
    val idxE = (0 until vLength).toList.find(i => nlAgent.variableName(i) == explListName.toUpperCase())
    
    (experimenting: Double, qValues: List[QLAgent.QValue]) => {
      if (idxA.isDefined) 
        nlAgent.setVariable(idxA.get, LogoList.fromIterator(qValues.map(_.name).toIterator))
      if (idxQ.isDefined) 
        nlAgent.setVariable(idxQ.get, LogoList.fromIterator(qValues.map(q => Double.box(q.value)).toIterator))
      if (idxN.isDefined)
        nlAgent.setVariable(idxN.get, LogoList.fromIterator(qValues.map(q => Double.box(q.freq)).toIterator))
      if (idxE.isDefined)
        nlAgent.setVariable(idxE.get, Double.box(experimenting))
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
case class QLAgent(experimenting: Double, gamma: Double, qValuesMap: Map[String,QLAgent.QValue],
                   choiceAlg: (Double, List[QLAgent.QValue]) => String,
                   expDecay: Boolean, updateNLogo: (Double, List[QLAgent.QValue]) => Unit,
                   getNextExperimening: (Boolean, Iterable[QLAgent.QValue]) => Double) {
  
  def updated(alt:String, reward: Double) : QLAgent = {
    val newExperimenting = getNextExperimening(expDecay, qValuesMap.values)
    val qValues = qValuesMap.values.map(_.value)
    val maxValue = qValues.tail.foldLeft(qValues.first)(Math.max(_,_))
    val newQvalue = qValuesMap.getOrElse(alt, new QLAgent.QValue(alt, 0.0, 0.0, gamma, 0.0)).updated(reward, maxValue, expDecay)
    val newQvaluesMap = qValuesMap.updated(alt, newQvalue)
    updateNLogo(newExperimenting, newQvaluesMap.values.toList)
    QLAgent(newExperimenting, gamma, newQvaluesMap, choiceAlg, expDecay, updateNLogo, getNextExperimening)
  }
  
  def setAlternatives(alternatives: List[String]) = {
    val newQvaluesMap = alternatives.map(key => (key -> qValuesMap.getOrElse(key, new QLAgent.QValue(key, 0.0, 0.0, gamma, 0.0)))).toMap
    updateNLogo(experimenting, newQvaluesMap.values.toList)
    QLAgent(experimenting, gamma, newQvaluesMap, choiceAlg, expDecay, updateNLogo, getNextExperimening)
  }
      
  
  def choose(alternatives: List[String]): String = {
    choiceAlg(experimenting, alternatives.map(alt => qValuesMap.getOrElse(alt, new QLAgent.QValue(alt, 0.0, 0.0, gamma, 0.0))))
  }
  
  /**
   * after calling this function, the agent successively lowers the levels of exploration.
   */
  def startDecreasing() : QLAgent = 
    QLAgent(experimenting, gamma, qValuesMap, choiceAlg, true, updateNLogo, getNextExperimening)
    
}
