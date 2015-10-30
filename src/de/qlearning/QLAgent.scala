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
  class QValue(val state: Int, val id: Int, val freq: Double, val value: Double, val gamma: Double, val nDecay: Double) extends HasValue {
    
    def updated(amount: Double, maxValue: Double, withDecay:Boolean) = { 
      val newValue = value + (1.0 / (freq + 1.0)) * (amount + gamma * maxValue - value)
      new QValue(state, id, freq + 1.0, newValue, gamma, if (withDecay) nDecay + 1.0 else nDecay)
    }
    
  }

  /**
   * convenient constructors of the QLAgent
   */
  def apply(state:Int, exploration: String, experimenting: Double, gamma:Double, nlAgent: org.nlogo.agent.Agent): QLAgent = 
    new QLAgent(state, experimenting, gamma, Map[Int, Map[Int,QValue]](), 
        (exploration match {
          case "epsilon-greedy" => epsGreedy
          case "softmax" => softmax
        }), 
        false, findUpdateFunction(nlAgent), 
        (exploration match {
          case "epsilon-greedy" => epsGreedyExperimenting(experimenting, _:Iterable[QValue])
          case "softmax" => softmaxExperimenting(experimenting, _:Iterable[QValue])
        }))
  
  
  ////////////////////////////////////////
  // various decision making algorithms //
  ////////////////////////////////////////
  
  private val epsGreedy = (epsilon: Double, qValues: Iterable[QValue]) => {
    val rh = de.util.ThreadLocalRandomHelper.current
    if (rh.uniform.nextDoubleFromTo(0, 1) < epsilon) {
      rh.randomComponent(qValues).id
    } else {
      val maxima = maximum(qValues)
      if (maxima.length == 1) maxima.head.id else rh.randomComponent(maxima).id
    }
  }
  
  private val epsGreedyExperimenting = (initialEpsilon: Double, qValues: Iterable[QValue]) => {
    val totalN = qValues.foldLeft(1.0)((sum, q) => sum + q.nDecay)
    initialEpsilon / Math.log(totalN)
  }
  
  private val softmax = (temperature: Double, qValues: Iterable[QValue]) => {
    val rh = de.util.ThreadLocalRandomHelper.current
    val expForm = qValues.scanLeft((0, 0.0))((result, qva) => (qva.id, result._2 + scala.math.exp(qva.value / temperature))).tail
    val randomValue = rh.uniform.nextDoubleFromTo(0, expForm.last._2)
    expForm.find(randomValue < _._2).get._1
  }
  
  private val softmaxExperimenting = (initialTemp: Double, qValues: Iterable[QValue]) => {
    val totalN = qValues.foldLeft(1.0)((sum, q) => sum + q.nDecay)
    Math.max(initialTemp / Math.log(totalN) , 0.02) // there is some problem if temperature <= 0.02
  }
    
  //////////////////////////////////////////////////////////////////////////////
  // constructing a function that updates the variables of the Netlogo-agents //
  //////////////////////////////////////////////////////////////////////////////
  
  def findUpdateFunction(nlAgent: org.nlogo.agent.Agent) = {
    
    val vLength = nlAgent.variables.size
    
    val idxA = (0 until vLength).toList.find(i => nlAgent.variableName(i) == altListName.toUpperCase())
    val idxQ = (0 until vLength).toList.find(i => nlAgent.variableName(i) == qvListName.toUpperCase())
    val idxN = (0 until vLength).toList.find(i => nlAgent.variableName(i) == freqListName.toUpperCase())
    val idxE = (0 until vLength).toList.find(i => nlAgent.variableName(i) == explRateName.toUpperCase())
    val idxS = (0 until vLength).toList.find(i => nlAgent.variableName(i) == stateName.toUpperCase())
    
    (state: Int, experimenting: Double, qValues: Iterable[QLAgent.QValue]) => {
      if (idxA.isDefined) {
        nlAgent.setVariable(idxA.get, 
            LogoList.fromVector(Vector[AnyRef]() ++ qValues.map(q => LogoList.fromVector(Vector[AnyRef](Double.box(q.state), Double.box(q.id))))))
      }
      if (idxQ.isDefined) 
        nlAgent.setVariable(idxQ.get, LogoList.fromVector(Vector[AnyRef]() ++ qValues.map(q => Double.box(q.value))))
      if (idxN.isDefined)
        nlAgent.setVariable(idxN.get, LogoList.fromVector(Vector[AnyRef]() ++ qValues.map(q => Double.box(q.freq))))
      if (idxE.isDefined)
        nlAgent.setVariable(idxE.get, Double.box(experimenting))
      if (idxS.isDefined)
        nlAgent.setVariable(idxS.get, Double.box(state))
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
case class QLAgent(state:Int, experimenting: Double, gamma: Double, qValuesMap: Map[Int, Map[Int, QLAgent.QValue]],
                   choiceAlg: (Double, Iterable[QLAgent.QValue]) => Int,
                   expDecay: Boolean, updateNLogo: (Int, Double, Iterable[QLAgent.QValue]) => Unit,
                   getNextExperimening: (Iterable[QLAgent.QValue]) => Double) {
  
  /**
   * the map of Q-Values becomes updated and a new state is set
   */
  def updated(alt:Int, reward: Double, newState: Int) : QLAgent = {
    
    //println("the agent chose alternative " + alt + " in state " + state + " and ends up in state " + newState)
    
    val qValuesMapEntry = qValuesMap.getOrElse(state, Map[Int, QLAgent.QValue]())
    
    //println(qValuesMapEntry.values.foldLeft("")((a,b) => a + b.state + ", " + b.id + "; "))
    
    // the Q-values of the new state are needed to specify the next exploration-rate and the maximum Q-value of the next decision 
    val newStateQvalues = if(expDecay || gamma > 0.0) {
      if (newState == state) qValuesMapEntry.values else qValuesMap.getOrElse(newState, Map[Int, QLAgent.QValue]()).values
    } else Iterable[QLAgent.QValue]()
    
    // new exploration-rate only if decay is activated
    val newExperimenting = if (expDecay) {
      getNextExperimening(newStateQvalues)
    } else experimenting
    // maximal Q-values is only needed if gamma > 0
    val maxValue = if (gamma == 0.0){ 0.0 } else {
      val qValueValues = newStateQvalues.map(_.value)
      if (qValueValues.isEmpty) 0.0 else qValueValues.tail.foldLeft(qValueValues.first)(Math.max(_,_))
    }
    
    // update the Q-value of the current state-alternative pair
    val newQvalue = qValuesMapEntry.getOrElse(alt, new QLAgent.QValue(state, alt, 0.0, 0.0, gamma, 0.0)).updated(reward, maxValue, expDecay)
    val newQValuesMap = qValuesMap.updated(state, qValuesMapEntry.updated(alt, newQvalue))
    
    updateNLogo(newState, newExperimenting, newQValuesMap.values.map(_.values).flatten)
    copy(state = newState, experimenting = newExperimenting, qValuesMap = newQValuesMap)
  }
  
  /**
   * a list of Integers is added to the Q-Values-Map for the current state
   */
  def setAlternatives(alternatives: List[Int]) ={
    val oldQvaluesMapEntry = qValuesMap.getOrElse(state, Map[Int, QLAgent.QValue]())
    val newQvaluesMap = qValuesMap.updated(state, 
        alternatives.map(key => (key -> oldQvaluesMapEntry.getOrElse(key, new QLAgent.QValue(state, key, 0.0, 0.0, gamma, 0.0)))).toMap)
    updateNLogo(state, experimenting, newQvaluesMap.values.map(_.values).flatten)
    copy(qValuesMap = newQvaluesMap)
  }
  
  /**
   * a new state is set
   */
  def setState(newState:Int) = copy(state = newState)
        
  /**
   * one of the elements of a list of integers is chosen given the choice-algorithm and the current Q-Values-Map
   */
  def choose(alternatives: List[Int]): Int = {
    val qValuesMapEntry = qValuesMap.getOrElse(state, alternatives.map(key => (key -> new QLAgent.QValue(state, key, 0.0, 0.0, gamma, 0.0))).toMap)
    val qValues = alternatives.map(key => qValuesMapEntry.getOrElse(key, new QLAgent.QValue(state, key, 0.0, 0.0, gamma, 0.0)))
    choiceAlg(experimenting, qValues)
  }
  
  /**
   * after calling this function, the agent successively lowers the levels of exploration.
   */
  def startDecreasing() : QLAgent = copy(expDecay = true)
    
}
