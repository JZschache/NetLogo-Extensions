package de.qlearning.util.games

import de.qlearning.util.Rational._
import de.qlearning.util.ThreadLocalRandomHelper

object Solver {
  
//  val pm1 = new PayoffMatrix(List[Rational](1,2,3,4,5,6), 3, 2)
//  val pm2 = new PayoffMatrix(List[Rational](7,8,9,10,11,12), 2, 3)
  
  val pm1 = new PayoffMatrix(List[Rational](0,6,2,5,3,3), 3, 2)
  val pm2 = new PayoffMatrix(List[Rational](1,0,4,0,2,3), 2, 3)
  
  val s = new Solver(pm1, pm2)
  s.run
  
  
}

//case class BasisVariable(indexSlack:Int, indexSolution: Int, tableauSlack: Array[Array[Rational]], tableauSolution: Array[Array[Rational]], slack:Boolean = true)

case class Variable(index:Int, isSlack: Boolean, colIndex:Int, tableauIndex: Int)
case class TableauRow(row: List[Rational], basis: Variable)

class Solver(pm1: PayoffMatrix, pm2: PayoffMatrix ) {
  
  val t1Index = 0
  val t2Index = 1
    
  val variables1 = (0 until pm1.nrow).map(i => (i -> (Variable(i, true, 1 + i, t1Index), Variable(i, false, 1 + pm1.ncol + i, t2Index)))).toMap
  val variables2 = (0 until pm2.nrow).map(i => (pm1.nrow + i -> (Variable(pm1.nrow + i, true, 1 + i, t2Index), Variable(pm1.nrow + i, false, 1 + pm2.ncol + i, t1Index)))).toMap
  val variables = variables1 ++ variables2
  
  
  def run() {
    
    val result = variables.values.flatMap(v => {
      
      val tab1 = pm1.getTableau((0 until pm1.nrow).map(i => variables1(i)._1).toList)
      val tab2 = pm2.getTableau((pm1.nrow until pm1.nrow + pm2.nrow).map(i => variables2(i)._1).toList)
      
      val newTableau = solve(v._2, Map(t1Index -> tab1, t2Index -> tab2))
    
      val n = normalise(newTableau)
      
      n :: n.map(j => {
        val basis = j._1
        val newTableau2 = if (basis.isSlack)
          solve(variables(basis.index)._2, newTableau)
        else
          solve(variables(basis.index)._1, newTableau)
        
        normalise(newTableau2)
      })
    }).toList
    
    
    
    (0 until result.size).foreach(i => {
      println(i + "  " + prettyPrint(result(i)))
    })
    
  }
  
  def normalise(tableau:Map[Int, List[TableauRow]]) = {
    
    val solution1 = tableau(t1Index).map(trow => if (trow.basis.isSlack) (trow.basis, new Rational(0)) else (trow.basis, trow.row(0))).toList
    val sum1 = solution1.foldLeft(new Rational(0))((sum, el) => sum + el._2)
    val solution2 = tableau(t2Index).map(trow => if (trow.basis.isSlack) (trow.basis, new Rational(0)) else (trow.basis, trow.row(0))).toList
    val sum2 = solution2.foldLeft(new Rational(0))((sum, el) => sum + el._2)
      
    val normSol = (solution1.map(pair => (pair._1, if (sum1 != 0) pair._2 / sum1 else pair._2)) ++ solution2.map(pair => (pair._1, if (sum2 != 0) pair._2 / sum2 else pair._2))).sort((s,t) => s._1.index < t._1.index)
      
      
    normSol
  }
  
  def prettyPrint(normSol: List[(Variable, Rational)]) {
    normSol.foldLeft("")((str, el) => {
      val p = el._2.pretty
      str + " | " + el._1.index + ": " + p + (0 until (6 - p.size)).foldLeft("")((str, el) => str + " ")
    })
  }
    
  def solve (startPivot: Variable, tableau: Map[Int, List[TableauRow]]) = {

    var (varOut, newTableau) = step(startPivot, tableau)
    var newPivot = if (varOut.isSlack) variables(varOut.index)._2 else variables(varOut.index)._1
    
    while (newPivot.index != startPivot.index) {
      step(newPivot, newTableau) match {
        case (varOut, nt) =>
          newTableau = nt
          newPivot = if (varOut.isSlack) variables(varOut.index)._2 else variables(varOut.index)._1
      }
    }
    newTableau
  }
  
  def step(pivot: Variable, tableaus: Map[Int,List[TableauRow]]) = {

    val tableau = tableaus(pivot.tableauIndex)
    val colIndex = pivot.colIndex
    
    val clashing = tableau.filter(trow => trow.row(colIndex) != 0)
    val firstClash = clashing.first
  
    // tableau row with base-variable that will be removed
    val tRowOut = clashing.tail.foldLeft((firstClash.row(colIndex) / firstClash.row(0), firstClash))((result, trow) => {
      val ratio = trow.row(colIndex) / trow.row(0)
      if (ratio < result._1) (ratio, trow) else result
    })._2
    
    val clashRow = tRowOut.row
    val colOutIndex = tRowOut.basis.colIndex
      
    val oldFactor = clashRow(colIndex)
    val newClashRow = (0 until clashRow.length).map(i => 
      if ( i == colOutIndex) 
        1 / oldFactor
      else if (i == colIndex)
        new Rational(0)
      else
        clashRow(i) / oldFactor * (-1)
    ).toList
  
    val newTableau = (0 until tableau.length).map(j => {
      if (j == tableau.indexOf(tRowOut) )
        TableauRow(newClashRow, pivot)
      else {
        val trow = tableau(j)
        if (trow.row(colIndex) != 0) {
          val oldFactor = trow.row(colIndex)
          val newRow = (0 until trow.row.length).map(i => 
            if (i == colIndex) 
              new Rational(0)
            else
              trow.row(i) + oldFactor * newClashRow(i)
            ).toList
          TableauRow(newRow, trow.basis)
        } else trow
      }
    }).toList
     
    (tRowOut.basis, tableaus.updated(pivot.tableauIndex, newTableau))
  }

  
}




class PayoffMatrix(val content: List[Rational], val nrow:Int, val ncol:Int) {
  require(content.length == nrow * ncol)
  
//  val ncol = content.length / nrow
  
  def getRow(row:Int) = content.drop(row * ncol).take(ncol)
  
  def getTableau(basis: List[Variable]): List[TableauRow] = {
//    val tableau: Array[Array[Rational]] = new Array[Array[Rational]](nrow)
//    for (i <- 0 until nrow) {
//      tableau(i) = Array[Rational](1) ++ (0 until nrow).map(_ => new Rational(0)) ++ getRow(i).map(e => e * -1)
//    }
    
    ((0 until nrow) zip basis).map(pair => 
      TableauRow(List[Rational](1) ++ (0 until nrow).map(_ => new Rational(0)) ++ getRow(pair._1).map(e => e * -1), pair._2)
    ).toList
    
  }
  
}