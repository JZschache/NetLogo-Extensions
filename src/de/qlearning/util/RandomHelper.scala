package de.qlearning.util
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import cern.jet.random.engine.RandomEngine
import cern.jet.random.Uniform
import cern.jet.random.engine.MersenneTwister64
import scala.compat.Platform


class RandomHelper {

  val generator: RandomEngine  = new MersenneTwister64(Platform.currentTime.toInt)
  val uniform = new Uniform(generator)

   /** Returns a new collection of the same type in a randomly chosen order.
   *
   *  @param  coll    the TraversableOnce to shuffle
   *  @return         the shuffled TraversableOnce
   */
  def shuffle[T, CC[X] <: TraversableOnce[X]](xs: CC[T])(implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T] = {
    val buf = new ArrayBuffer[T] ++= xs

    def swap(i1: Int, i2: Int) {
      val tmp = buf(i1)
      buf(i1) = buf(i2)
      buf(i2) = tmp
    }

    for (n <- buf.length to 2 by -1) {
      val k = uniform.nextIntFromTo(0,(n-1))
      swap(n - 1, k)
    }

    bf(xs) ++= buf result
  }
  
  def randomComponent[T](xs:Traversable[T]): T = {
    try{
      val k = uniform.nextIntFromTo(0, xs.size - 1)
      (xs drop k).head
    } catch {
      case e: Exception => {

      }
      xs.head
    }
  }
}