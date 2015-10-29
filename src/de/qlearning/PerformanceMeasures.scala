package de.qlearning

import akka.agent.{ Agent => AkkaAgent }
import de.util.PerformanceMeasure
import org.nlogo.api.ScalaConversions._
import org.nlogo.api.DefaultReporter
import org.nlogo.api.Syntax._
import org.nlogo.api.Argument
import org.nlogo.api.Context

class PerformanceMeasures extends EmptyPerformanceMeasures{
  import QLSystem._

  // various AkkaAgents that measure performance of the program
  val hundredTicksPerf = AkkaAgent(PerformanceMeasure())
  val nlSuperIdlePerf = AkkaAgent(PerformanceMeasure())
  val nlSuperHandleGroupsPerf = AkkaAgent(PerformanceMeasure())
  val nlSuperUpdatePerf = AkkaAgent(PerformanceMeasure())
  val headlessIdlePerf = AkkaAgent(Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure()))
  val headlessHandleGroupsPerf = AkkaAgent(Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure()))
  val headlessHandleChoicesPerf = AkkaAgent(Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure()))
  val headlessAnsweringPerf = AkkaAgent(Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure()))
  
  override def init() {
    hundredTicksPerf update PerformanceMeasure()
    nlSuperIdlePerf update PerformanceMeasure()
    nlSuperHandleGroupsPerf update PerformanceMeasure()
    nlSuperUpdatePerf update PerformanceMeasure()
    headlessIdlePerf update Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure()) 
    headlessHandleGroupsPerf update Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure())
    headlessHandleChoicesPerf update Map[Int,PerformanceMeasure]().withDefault(id => PerformanceMeasure())
  }
  
  override def startHundredTicksPerf(time: Long) = hundredTicksPerf send { _.start(time)}
  override def stopHundredTicksPerf(time: Long) = hundredTicksPerf send { _.end(time)}
  
  override def startNlSuperIdlePerf(time: Long) = nlSuperIdlePerf send { _.start(time)}
  override def stopNlSuperIdlePerf(time: Long) = nlSuperIdlePerf send { _.end(time)}
  
  override def startNlSuperHandleGroupsPerf(time: Long) = nlSuperHandleGroupsPerf send { _.start(time)}
  override def stopNlSuperHandleGroupsPerf(time: Long) = nlSuperHandleGroupsPerf send { _.end(time)}
  
  override def startNlSuperUpdatePerf(time: Long) = nlSuperUpdatePerf send { _.start(time)}
  override def stopNlSuperUpdatePerf(time: Long) = nlSuperUpdatePerf send { _.end(time)}
  
  override def startHeadlessIdlePerf(id:Int, time: Long) = headlessIdlePerf send { m => m.updated(id, m(id).start(time)) }
  override def stopHeadlessIdlePerf(id:Int, time: Long) = headlessIdlePerf send { m => m.updated(id, m(id).end(time)) }
  
  override def startHeadlessHandleGroupsPerf(id:Int, time: Long) = headlessHandleGroupsPerf send { m => m.updated(id, m(id).start(time)) }
  override def stopHeadlessHandleGroupsPerf(id:Int, time: Long) = headlessHandleGroupsPerf send { m => m.updated(id, m(id).end(time)) }
  
  override def startHeadlessHandleChoicesPerf(id:Int, time: Long) = headlessHandleChoicesPerf send { m => m.updated(id, m(id).start(time)) }
  override def stopHeadlessHandleChoicesPerf(id:Int, time: Long) = headlessHandleChoicesPerf send { m => m.updated(id, m(id).end(time)) }
  
  override def startHeadlessAnsweringPerf(id:Int, time: Long) = headlessAnsweringPerf send { m => m.updated(id, m(id).start(time)) }
  override def stopHeadlessAnsweringPerf(id:Int, time: Long) = headlessAnsweringPerf send { m => m.updated(id, m(id).end(time)) }
  
  override def getHundredTicksPerf = hundredTicksPerf.get.average.toLogoObject
  
  override def getNlSuperIdlePerf = nlSuperIdlePerf.get.average.toLogoObject
  override def getNlSuperHandleGroupsPerf = nlSuperHandleGroupsPerf.get.average.toLogoObject
  override def getNlSuperUpdatePerf = nlSuperUpdatePerf.get.average.toLogoObject
  
  override def getHeadlessIdlePerf(id:Int) = headlessIdlePerf.get.apply(id).average.toLogoObject
  override def getHeadlessHandleGroupsPerf(id:Int) = headlessHandleGroupsPerf.get.apply(id).average.toLogoObject
  override def getHeadlessHandleChoicesPerf(id:Int) = headlessHandleChoicesPerf.get.apply(id).average.toLogoObject
  override def getHeadlessAnsweringPerf(id:Int) = headlessAnsweringPerf.get.apply(id).average.toLogoObject
}

class EmptyPerformanceMeasures {
  
  def init() {}
  
  def startHundredTicksPerf(time: Long) {}
  def stopHundredTicksPerf(time: Long) {}
  
  def startNlSuperIdlePerf(time: Long) {}
  def stopNlSuperIdlePerf(time: Long) {}
  
  def startNlSuperHandleGroupsPerf(time: Long) {}
  def stopNlSuperHandleGroupsPerf(time: Long) {}
  
  def startNlSuperUpdatePerf(time: Long) {}
  def stopNlSuperUpdatePerf(time: Long) {}
  
  def startHeadlessIdlePerf(id:Int, time: Long) {}
  def stopHeadlessIdlePerf(id:Int, time: Long) {}
  
  def startHeadlessHandleGroupsPerf(id:Int, time: Long) {}
  def stopHeadlessHandleGroupsPerf(id:Int, time: Long) {}
  
  def startHeadlessHandleChoicesPerf(id:Int, time: Long) {}
  def stopHeadlessHandleChoicesPerf(id:Int, time: Long) {}
  
  def startHeadlessAnsweringPerf(id:Int, time: Long) {}
  def stopHeadlessAnsweringPerf(id:Int, time: Long) {}
  
  def getHundredTicksPerf = toLogoObject(0.0)
  
  def getNlSuperIdlePerf = toLogoObject(0.0)
  def getNlSuperHandleGroupsPerf = toLogoObject(0.0)
  def getNlSuperUpdatePerf = toLogoObject(0.0)
  
  def getHeadlessIdlePerf(id:Int) = toLogoObject(0.0)
  def getHeadlessHandleGroupsPerf(id:Int) = toLogoObject(0.0)
  def getHeadlessHandleChoicesPerf(id:Int) = toLogoObject(0.0)
  def getHeadlessAnsweringPerf(id:Int) = toLogoObject(0.0)
}

class GetPerformance extends DefaultReporter {
  override def getAgentClassString = "O"    
  override def getSyntax = reporterSyntax(Array[Int](StringType), NumberType)
  def report(args: Array[Argument], c: Context): AnyRef = { 
    val s = args(0).getString
    val strings = s.split(' ')
    if (strings.size == 2)  {
      val id = strings(1).toInt - 1
      strings(0) match {
        case "HeadlessIdle" => 
          QLSystem.perfMeasures.getHeadlessIdlePerf(id)
        case "HeadlessHandleGroups" =>
          QLSystem.perfMeasures.getHeadlessHandleGroupsPerf(id)
        case "HeadlessHandleChoices" => 
          QLSystem.perfMeasures.getHeadlessHandleChoicesPerf(id)
        case "HeadlessAnswering" => 
          QLSystem.perfMeasures.getHeadlessAnsweringPerf(id)
      }
    } else {
      strings(0) match {
        case "NLSuperIdle" =>
          QLSystem.perfMeasures.getNlSuperIdlePerf
        case "NLSuperHandleGroups" => 
          QLSystem.perfMeasures.getNlSuperHandleGroupsPerf
        case "NLSuperUpdate" =>
          QLSystem.perfMeasures.getNlSuperUpdatePerf
        case "HundredTicks" =>
          QLSystem.perfMeasures.getHundredTicksPerf
      }
    }
  }
}
