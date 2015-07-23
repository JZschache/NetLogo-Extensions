package de.util


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