package de.games

import de.util.Rational._

object Optima {
  
  
  
  val pairList: List[(Rational,Rational)] = List((1,1), (2,3), (0,3), (3,0), (3,1), (2,3))
  val mpf = new MaximalPairFinder(pairList)
  mpf.maxima
  mpf.maximaIndices
  
}

class MaximalPairFinder(vectors: List[(Rational, Rational)]) {
  
  val sorted = vectors.sort((v1, v2) => (v1._1 > v2._1) || (v1._1 == v2._1 && v1._2 >= v2._2))
  
  val (maximaList, isMaxima) = sorted.foldLeft((List[(Rational, Rational)](), Map[(Rational, Rational), Boolean]()))((r, el) => {
    if (r._1.exists(p => (p._2 > el._2 || (p._2 == el._2 && p._1 > el._1) )))
      (r._1, r._2.updated(el, false))
    else
      (el :: r._1, r._2.updated(el, true))
  })
  
  def maxima = maximaList
  
  def maximaIndices = vectors.map(v => isMaxima(v))
  
}



class MaximalVectorFinder(vectors: List[List[Rational]]) {
  
  
  
  
  
}
