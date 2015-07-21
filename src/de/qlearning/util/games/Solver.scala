package de.qlearning.util.games

import de.qlearning.util.Rational._
import org.nlogo.extensions.matrix._
import Jama.Matrix

object Solver {
  
  val pm1 = new PayoffMatrix(List[Rational](1,2,3,4,5,6), 3, 2)
  val pm2 = new PayoffMatrix(List[Rational](7,8,9,10,11,12), 2, 3)
  
  
  
}

case class BasisVariable(indexSlack:Int, indexSolution: Int, slack:Boolean = true)

class Solver(pm1: PayoffMatrix, pm2: PayoffMatrix ) {
    
//  val tableau1 = Array.ofDim[Rational](pm1.nrow, 2 + pm1.nrow + pm1.ncol)
  
  // the slacks of player 1 and the solutions of player 2
  val tableau1 = pm1.getTableau
  // the slacks of player 2 and the solutions of player 1
  val tableau2 = pm2.getTableau
  
  // the slacks and the solutions of player 1
  val variables1 = (0 until pm1.nrow).map(i => BasisVariable( 1 + i, 1 + pm1.ncol + i))
  // the slacks and the solutions of player 2
  val variables2 = (0 until pm2.nrow).map(i => BasisVariable(1 + i, 1 + pm2.ncol + i))
  
  // init the basis
  val basis = variables1 ++ variables2
  
  //TODO: algorithm
  
  // solution is first colum??
  
  // set zeros if not in basis
//  variables1.foreach(b => {
//    if (b.slack)
//      tableau2.foreach(row => row(b.indexSolution) = 0)
//    else
//      tableau1.foreach(row => row(b.indexSlack) = 0)
//  })
//  variables2.foreach(b => {
//    if (b.slack)
//      tableau1.foreach(row => row(b.indexSolution) = 0)
//    else
//      tableau2.foreach(row => row(b.indexSlack) = 0)
//  })
//  
//  
//  
//  val m = new Matrix(tableau1.reduce((A1, A2) => A1 ++ A2).map(_.getDouble), pm1.nrow)
//  
//  m.solve(new Matrix(m.getRowDimension(), 1, 0.0))
  
}



class PayoffMatrix(val content: List[Rational], val nrow:Int, val ncol:Int) {
  require(content.length == nrow * ncol)
  
//  val ncol = content.length / nrow
  
  def getRow(row:Int) = content.drop(row * ncol).take(ncol)
  
  def getTableau: Array[Array[Rational]] = {
    val tableau: Array[Array[Rational]] = (new Array[Array[Rational]](nrow): Array[Array[Rational]])
    for (i <- 0 until nrow) {
      tableau(i) = Array[Rational](1) ++ (0 until nrow).map(_ => new Rational(0)) ++ getRow(i).map(e => e * -1)
    }
    tableau
  }
  
}