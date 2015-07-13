package de.qlearning.util

case class Average(sum: Double = 0.0, count: Double = 0.0){
  
  def +(toAdd:Double) = new Average(sum + toAdd, count + 1.0)
//  def +(toAdd:Average) = new Average(sum + toAdd.sum, count + toAdd.count)
  def -(toSub:Double) = new Average(sum - toSub, count - 1.0)
//  def -(toSub:Average) = new Average(sum - toSub.sum, count - toSub.count)
  def incSum(toAdd:Double) = new Average(sum + toAdd, count)
  def decSum(toSub:Double) = new Average(sum - toSub, count)
  
  def average = if (count > 0.0) (math rint sum / count * 10) / 10 else 0.0
  
}

case class PerformanceMeasure(startTime:Long = 0, sum: Double = 0.0, count: Double = 0.0) {
  
  def start(time: Long) = PerformanceMeasure(time, sum, count)
  
  def end(time: Long) = {
    if (startTime != 0) {
      val duration = (time - startTime).toDouble
      PerformanceMeasure(0, sum + duration, count + 1.0)
    } else 
      PerformanceMeasure(0, sum, count)
  }
  
  def average = if (count > 0.0) (math rint sum / count * 100) / 100 else 0.0 
  
}