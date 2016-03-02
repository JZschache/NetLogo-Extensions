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
  
  sealed trait QValue extends HasValue { val state: Int; val id: Int; val freq: Double; val value: Double} 
    
  /**
   *  the QValue (immutable) as used by Q-learning
   */ 
  class QValueAvg(val state: Int, val id: Int, val freq: Double, val value: Double, val gamma: Double) extends QValue {
    
    def updated(amount: Double, maxValue: Double) = { 
      val newValue = value + (1.0 / (freq + 1.0)) * (amount + gamma * maxValue - value)
      new QValueAvg(state, id, freq + 1.0, newValue, gamma)
    }
    
  }
  
  /**
   *  the QValue (immutable) as used by Roth-Erev
   */ 
  class QValueSum(val state: Int, val id: Int, val freq: Double, val value: Double, val sum: Double) extends QValue {
    
    def updatedChosen(amount: Double, epsilon: Double) = {
      //println("new Sum (chosen): " + (value + (1.0 - epsilon) * amount))
      new QValueSum(state, id, freq + 1.0, value + (1.0 / (freq + 1.0)) * (amount - value), sum + (1.0 - epsilon) * amount)
    }
    
    def updatedUnchosen(amount: Double, epsilon: Double) = {
      //println("new Sum (unchosen): " + (value + epsilon * amount))
      new QValueSum(state, id, freq, value, sum + epsilon * amount)
    }
        
  }

  /**
   * convenient constructors of the QLAgent
   */
  def apply(state:Int, exploration: String, experimenting: Double, gamma:Double, nlAgent: org.nlogo.agent.Agent): QLAgent = {
    
    val vLength = nlAgent.variables.size
    val idxA = (0 until vLength).toList.find(i => nlAgent.variableName(i) == altListName.toUpperCase())
    val idxQ = (0 until vLength).toList.find(i => nlAgent.variableName(i) == qvListName.toUpperCase())
    val idxN = (0 until vLength).toList.find(i => nlAgent.variableName(i) == freqListName.toUpperCase())
    val idxE = (0 until vLength).toList.find(i => nlAgent.variableName(i) == explRateName.toUpperCase())
    val idxS = (0 until vLength).toList.find(i => nlAgent.variableName(i) == stateName.toUpperCase())

    // update Function for exploration-rate variable
    val expUpdateFun = if (idxE.isDefined)
        (experimenting: Double) => nlAgent.setVariable(idxE.get, Double.box(experimenting))
      else
        (_:Double) => {}
    // update Function for all other variables
    val updateFun = (state: Int, qValues: Iterable[QLAgent.QValue]) => {
      if (idxA.isDefined) {
        nlAgent.setVariable(idxA.get, 
            LogoList.fromVector(Vector[AnyRef]() ++ qValues.map(q => LogoList.fromVector(Vector[AnyRef](Double.box(q.state), Double.box(q.id))))))
      }
      if (idxQ.isDefined) 
        nlAgent.setVariable(idxQ.get, LogoList.fromVector(Vector[AnyRef]() ++ qValues.map(q => Double.box(q.value))))
      if (idxN.isDefined)
        nlAgent.setVariable(idxN.get, LogoList.fromVector(Vector[AnyRef]() ++ qValues.map(q => Double.box(q.freq))))
      if (idxS.isDefined)
        nlAgent.setVariable(idxS.get, Double.box(state))
    }
    
    new QLAgent(state, Map[Int, Map[Int,QValue]](), 
        (exploration match {
          case "epsilon-greedy" => epsGreedy(experimenting, expUpdateFun, _:Double, _:Iterable[QValue])
          case "softmax" => softmax(experimenting, expUpdateFun, _:Double, _:Iterable[QValue])
          case "Roth-Erev" => rothErev
        }), 
        false, Map[Int, Double](), updateFun, 
        (exploration match {
          case "epsilon-greedy" => (state: Int, alt: Int) => new QValueAvg(state, alt, 0.0, 0.0, gamma)
          case "softmax" => (state: Int, alt: Int) => new QValueAvg(state, alt, 0.0, 0.0, gamma)
          case "Roth-Erev" => (state: Int, alt: Int) => new QValueSum(state, alt, 0.0, 0.0, 1.0)
        }),
        (exploration match {
          case "epsilon-greedy" => updatedQValues(gamma, _:Int, _: Int, _: Double, _: Int, _: Map[Int, Map[Int, QLAgent.QValue]], _: Double)
          case "softmax" => updatedQValues(gamma, _:Int, _: Int, _: Double, _: Int, _: Map[Int, Map[Int, QLAgent.QValue]], _: Double)
          case "Roth-Erev" => updatedQValuesRE(experimenting, expUpdateFun, _:Int, _: Int, _: Double, _: Int, _: Map[Int, Map[Int, QLAgent.QValue]], _: Double)
        })
        )
    }
  
  
  ////////////////////////////////////////
  // various decision making algorithms //
  ////////////////////////////////////////
  
  private val epsGreedy = (epsilon:Double, expUpdateFun: Double => Unit, nDecay: Double, qValues: Iterable[QValue]) => {
    val rh = de.util.ThreadLocalRandomHelper.current
    val threshold = if (nDecay > 1) {
      val newEps = epsilon / Math.log(nDecay)
      expUpdateFun(newEps)
      newEps
    } else epsilon
    if (rh.uniform.nextDoubleFromTo(0, 1) < threshold) {
      rh.randomComponent(qValues).id
    } else {
      val maxima = maximum(qValues)
      if (maxima.length == 1) maxima.head.id else rh.randomComponent(maxima).id
    }
  }
  
  private val softmax = (temperature: Double, expUpdateFun: Double => Unit, nDecay: Double, qValues: Iterable[QValue]) => {
    val rh = de.util.ThreadLocalRandomHelper.current
    val threshold = if (nDecay > 1) {
      val newTemp = Math.max(temperature / Math.log(nDecay) , 0.02)
      expUpdateFun(newTemp)
      newTemp
    } else temperature
    val expForm = qValues.scanLeft((0, 0.0))((result, qva) => (qva.id, result._2 + scala.math.exp(qva.value / threshold))).tail
    val randomValue = rh.uniform.nextDoubleFromTo(0, expForm.last._2)
    expForm.find(randomValue < _._2).get._1
  }
  
  private val rothErev = (epsilon: Double, qValues: Iterable[QValue])  => {
    val rh = de.util.ThreadLocalRandomHelper.current
    val sumForm = qValues.scanLeft((0, 0.0))((result, qva) => (qva.id, result._2 + Math.max(qva.asInstanceOf[QValueSum].sum, 0))).tail
    val randomValue = rh.uniform.nextDoubleFromTo(0, sumForm.last._2)
    //println("sumForm: " + sumForm)
    try {
      sumForm.find(randomValue <= _._2).get._1
    } catch {
      case ex: java.util.NoSuchElementException => {
        println("sumForm: " + sumForm)
        println("randomValue: " + randomValue)
        println("sumForm.find(randomValue < _._2): " + sumForm.find(randomValue < _._2))
        throw ex
      }
    }
  }
    
  ////////////////////////////////////////////////////////
  // constructing a function that updates the Q-values  //
  ////////////////////////////////////////////////////////
  
  private val updatedQValues = (gamma:Double, state:Int, alt: Int, reward: Double, newState: Int, qValuesMap: Map[Int, Map[Int, QLAgent.QValue]], nDecay: Double) => {
    val qValuesMapEntry = qValuesMap.getOrElse(state, Map[Int, QLAgent.QValue]())
    
    val maxValue = if (gamma == 0.0){ 0.0 } else {
      val newStateQvalues = if (newState == state) qValuesMapEntry.values else qValuesMap.getOrElse(newState, Map[Int, QLAgent.QValue]()).values
      if (newStateQvalues.isEmpty) 0.0 else newStateQvalues.tail.foldLeft(newStateQvalues.first.value)((a,b) => Math.max(a,b.value))
    }
    
    val newQvalue = qValuesMapEntry.getOrElse(alt, new QValueAvg(state, alt, 0.0, 0.0, gamma)).asInstanceOf[QValueAvg].updated(reward, maxValue)
    qValuesMap.updated(state, qValuesMapEntry.updated(alt, newQvalue))
  }
  
  /**
   * special update is needed for Roth-Erev algorithm
   */
  private val updatedQValuesRE = (epsilon:Double, expUpdateFun: Double => Unit, state:Int, alt: Int, reward: Double, newState: Int, qValuesMap: Map[Int, Map[Int, QLAgent.QValue]], nDecay: Double) => {
    val qValuesMapEntry = qValuesMap.getOrElse(state, Map[Int, QLAgent.QValue]())
    
    val threshold = if (nDecay > 1.0) {
      val newEps = epsilon / Math.log(nDecay)
      expUpdateFun(newEps)
      newEps
    } else epsilon
    
    val weight = if (qValuesMapEntry.contains(alt)) {
      threshold / (qValuesMapEntry.size.toDouble - 1.0)
    } else {
      threshold / qValuesMapEntry.size.toDouble
    }
    
    qValuesMap.updated(state, qValuesMapEntry.map(pair => (pair._1, if (pair._1 == alt) {
      pair._2.asInstanceOf[QValueSum].updatedChosen(reward, threshold)
    } else {
      pair._2.asInstanceOf[QValueSum].updatedUnchosen(reward, weight)
    })))
  }
  
}

/**
 *  a QLAgent holds information about the temporary state of an Agent
 *  
 *  it is immutable
 *  
 *  it is used to update the Q-values and to get a decision from the agent
 *  
 *  @param	state				the current state of the environment
 *  @param	qValuesMap			the Q-Values mapped by state and alternative
 *  @param	choiceAlg			a function that takes the nDecay of the current state and a list of alternatives. it returns one of the alternatives (epsilon-greedy, softmax, ..).
 *  @param	expDecy				flag that indicates whether nDecay increments
 *  @param	nDecayMap			contains the nDecay (number of choices) for each state
 *  @param	updateNLogo			a function that updates almost all NLAgent variable (exploration rate is updated in choiceAlg or in updatedQValuesMap)
 *  @param	newQValue			a function that returns the appropriate instance of QValue (QValueAvg or QValueSum)
 *  @param	updateQValuesMap	a function that updates the qValuesMap (this is different for Roth-Erev than for Q-learning)
 */ 
case class QLAgent(state:Int, qValuesMap: Map[Int, Map[Int, QLAgent.QValue]], 
                   choiceAlg: (Double, Iterable[QLAgent.QValue]) => Int,
                   expDecay: Boolean, nDecayMap: Map[Int, Double],
                   updateNLogo: (Int, Iterable[QLAgent.QValue]) => Unit,
                   newQValue: (Int, Int) => QLAgent.QValue,
                   updatedQValuesMap: (Int, Int, Double, Int, Map[Int, Map[Int, QLAgent.QValue]], Double) => Map[Int, Map[Int, QLAgent.QValue]]) {
  
  /**
   * The map of Q-Values becomes updated and a new state is set. It is called after a reward is obtained.
   */
  def updated(alt:Int, reward: Double, newState: Int) : QLAgent = {
    val nDecay = nDecayMap.getOrElse(state, 1.0)
    val newQValuesMap = updatedQValuesMap(state, alt, reward, newState, qValuesMap, nDecay)
    updateNLogo(newState, newQValuesMap.values.map(_.values).flatten)
    copy(state = newState, qValuesMap = newQValuesMap, nDecayMap = if (expDecay) nDecayMap.updated(state, nDecay + 1) else nDecayMap)
  }
  
  /**
   * A list of Integers is added to the Q-Values-Map for the current state. 
   * It is called if a QLAgent is generated with alternatives or if a group structure is created.
   */
  def setAlternatives(alternatives: List[Int]) ={
    val oldQvaluesMapEntry = qValuesMap.getOrElse(state, Map[Int, QLAgent.QValue]())
    val newQvaluesMap = qValuesMap.updated(state, 
        alternatives.map(key => (key -> oldQvaluesMapEntry.getOrElse(key, newQValue(state, key)))).toMap)
    updateNLogo(state, newQvaluesMap.values.map(_.values).flatten)
    copy(qValuesMap = newQvaluesMap)
  }
    
  /**
   * One of the elements of a list of integers is chosen given the choice-algorithm and the current Q-Values-Map. 
   * If some of the alternatives are not in qValuesMap, default QValues are generated. 
   * But no QValues are added to qValuesMap unless the alternative is actually chosen (this may discriminate against Roth-Erev). 
   */
  def choose(alternatives: List[Int]): Int = {
    val qValuesMapEntry = qValuesMap.getOrElse(state, Map[Int, QLAgent.QValue]())
    val qValues = alternatives.map(key => qValuesMapEntry.getOrElse(key, newQValue(state, key)))
    val choice = choiceAlg(nDecayMap.getOrElse(state, 1.0), qValues)
    //println(choice)
    choice
  }
  
  /**
   * After calling this function, the agent successively increases the nDecays and, hence, lowers the levels of exploration.
   */
  def startDecreasing() : QLAgent = copy(expDecay = true)
    
}
