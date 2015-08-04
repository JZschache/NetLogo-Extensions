package de.util

object Rational {
  
  val accuracy:Int = 1000000

  implicit def intToRational(x: Int) = new Rational(x)

  class Rational(numer: Int, denom: Int)  {
	require(denom != 0)
	  
	private val g = gcd(numer.abs, denom.abs)
	val factor = if((numer > 0 && denom < 0) || (numer < 0 && denom < 0)) -1 else 1
	val n = numer / g * factor
	val d = if (n == 0) 1 else denom / g * factor
	
	def this(number:Int) = this(number, 1)
	def this(number: Double) = this(Math.floor(number * accuracy).toInt, accuracy)
	 
	override def toString = n + "/" + d
	  
	def + (that: Rational) = new Rational(n * that.d + that.n * d, d * that.d)
	def + (i: Int) = new Rational(n + i * d, d)
	  
	def - (that: Rational) = new Rational(n * that.d - that.n * d, d * that.d)
	def - (i: Int) = new Rational(n - i * d, d)
	  
	def * (that: Rational) = new Rational(n * that.n, d * that.d)
	def * (i: Int) = new Rational(n * i, d)
	  
	def / (that: Rational) = new Rational(n * that.d, d * that.n)
	def / (i: Int) = new Rational(n, d * i)
	  
	def < (that: Rational) = n * that.d < that.n * d
	def <= (that: Rational) = n * that.d <= that.n * d
	def > (that: Rational) = n * that.d > that.n * d
	def >= (that: Rational) = n * that.d >= that.n * d
	
	def != (that: Rational) = n * that.d != that.n * d
	def != (i: Int) = n != d * i
	
	def == (that: Rational) = n * that.d == that.n * d
	def == (i: Int) = n == d * i
	
	private def gcd(a:Int, b:Int):Int  = if ( b == 0) a else gcd(b, a % b)
	
	def getDouble: Double = n.toDouble / d.toDouble
	
	def pretty = {
	  if (n == 0) "0" else {if (d.abs == 1) (n / d).toString else n + "/" + d}
	}
	
	def prettyDouble(decimal:Int) = {
	  val t = Math.pow(10.0, decimal.toDouble)
	  ((Math.floor(t * n.toDouble / d.toDouble) / t))
	}
	
	def floor = Math.floor(n.toDouble / d.toDouble).toInt
	
  }

  //
//case class Average(sum: Double = 0.0, count: Double = 0.0){
//  
//  def +(toAdd:Double) = new Average(sum + toAdd, count + 1.0)
////  def +(toAdd:Average) = new Average(sum + toAdd.sum, count + toAdd.count)
//  def -(toSub:Double) = new Average(sum - toSub, count - 1.0)
////  def -(toSub:Average) = new Average(sum - toSub.sum, count - toSub.count)
//  def incSum(toAdd:Double) = new Average(sum + toAdd, count)
//  def decSum(toSub:Double) = new Average(sum - toSub, count)
//  
//  def average = if (count > 0.0) (math rint sum / count * 10) / 10 else 0.0
//  
//}
  
}