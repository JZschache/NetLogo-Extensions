package de.games

import de.util.Rational._

object Solver {
  
  val pm1 = new PayoffMatrix(List[Rational](0,6,2,5,3,3), 3, 2)
  val pm2 = new PayoffMatrix(List[Rational](1,0,0,2,4,3), 3, 2)
  
  val s = new LemkeHowsonSolver(pm1, pm2)
  val sol = s.run
  
  (0 until sol.size).foreach(i => {
      println(i + "  " + prettyPrint(sol(i)))
    })
  
  def prettyPrint(normSol: List[(Variable, Rational)]) = {
    normSol.foldLeft("")((str, el) => {
      val p = el._2.pretty
      str + " | " + el._1.index + ": " + p + (0 until (6 - p.size)).foldLeft("")((str, el) => str + " ")
    })
  }
}

case class Variable(index:Int, isSlack: Boolean, colIndex:Int, tableauIndex: Int)
case class TableauRow(row: List[Rational], basis: Variable)

class LemkeHowsonSolver(pm1: PayoffMatrix, pm2: PayoffMatrix ) {
  
  // the variables (as pairs of slack and non-slack variables)
  val variables1 = (0 until pm1.nrow).map(i => (i -> (Variable(i, true, 1 + i, 0), Variable(i, false, 1 + pm1.ncol + i, 1)))).toMap
  val variables2 = (0 until pm2.ncol).map(i => (pm1.nrow + i -> (Variable(pm1.nrow + i, true, 1 + i, 1), Variable(pm1.nrow + i, false, 1 + pm2.nrow + i, 0)))).toMap
  val variables = variables1 ++ variables2
  
  // the equation systems with slack variables as basis (called 'tableau' Codenotti et al )
  val tab1 = pm1.getTableau((0 until pm1.nrow).map(i => variables1(i)._1).toList, false)
  val tab2 = pm2.getTableau((pm1.nrow until pm1.nrow + pm2.ncol).map(i => variables2(i)._1).toList, true)
  
  /**
   * the algorithm is run for each non-slack variable and, 
   * from this point, again for variables that are not in the solution
   * 
   * it returns a list of solutions. 
   * a solution is a list of pairs, each pair containing a variable and a rational value (the probability)
   */
  def run() = {
    
    val result = variables.values.flatMap(v => {
         
      val solvedTableau1 = solve(v._2, List(tab1, tab2))
      
      val normSolution =  normalise(solvedTableau1)
      
      normSolution :: normSolution.map(j => {
        val basis = j._1
        val solvedTableau2 = if (basis.isSlack)
          solve(variables(basis.index)._2, solvedTableau1)
        else
          solve(variables(basis.index)._1, solvedTableau1)
        normalise(solvedTableau2)
      })
    }).toList
   
//    println("result:")
//    result.foreach(r => println(prettyPrint(r)))
     // filter the all zero solutions or solutions with a negative entry
    val filtered = result.filter(el => !(el.forall(_._2 == 0) || el.exists(_._2 < 0)))
//    println("filtered:")
//    filtered.foreach(r => println(prettyPrint(r)))
    
    val distinct = filtered.foldLeft(List[List[(Variable, Rational)]]())((result, el) => {
      if (result.exists(p => (p zip el).forall(pair => pair._1._2 == pair._2._2)))
        result
      else
        el :: result
    })
    distinct
  }
  
  def prettyPrint(normSol: List[(Variable, Rational)]) = {
    normSol.foldLeft("")((str, el) => {
      val p = el._2.pretty
      str + " | " + el._1.index + ": " + p + (0 until (6 - p.size)).foldLeft("")((str, el) => str + " ")
    })
  }
  
  object Break extends Exception { }
  /**
   * Finds a solution to the tableau starting with startPivot.
   */
  def solve (startPivot: Variable, tableaus: List[List[TableauRow]]) = {

    var (varOut, newTableau) = step(startPivot, tableaus)
    var newPivot = if (varOut.isSlack) variables(varOut.index)._2 else variables(varOut.index)._1
    var count = 0 
    try {
      while (newPivot.index != startPivot.index) { // a solution has not been found yet
        count = count + 1
        if (count > 1000)
          throw Break
        val pair = step(newPivot, newTableau)
        newTableau = pair._2
        newPivot = if (pair._1.isSlack) variables(pair._1.index)._2 else variables(pair._1.index)._1
      }
    } catch  {
      case Break => tableaus
    } 
    newTableau
  }
  
//  def solve (pivot: Variable, tableaus: List[List[TableauRow]], startPivot: Variable):List[List[TableauRow]] = {
//    
//    val tableau = tableaus(pivot.tableauIndex)
//    val clashing = tableau.filter(trow => trow.row(pivot.colIndex) != 0)
//    
//    val clashRow = clashing.tail.foldLeft((clashing.first.row(0) / clashing.first.row(pivot.colIndex), clashing.first))((result, trow) => {
//      val ratio = trow.row(0) / trow.row(pivot.colIndex)
//      if (ratio > result._1) (ratio, trow) else result
//    })._2
//
//    val (varOut, newTableau) = step(pivot, clashRow, tableau)
//    val newTableaus = tableaus.updated(pivot.tableauIndex, newTableau)
//    val newPivot = if (varOut.isSlack) variables(varOut.index)._2 else variables(varOut.index)._1
//      
//    if (newPivot.index != startPivot.index) {
//      solve(newPivot, newTableaus, startPivot)
//    } else
//      newTableaus
//  }
  
  /**
   * A step of the algorithm.
   * 
   * It returns the variable that is leaving the basis and the new tableaus (one of them has not changes).
   */
  def step(pivot: Variable, tableaus: List[List[TableauRow]]) = {

    val tableau = tableaus(pivot.tableauIndex)
    
    // tableau row with base-variable that will be removed (min-ratio)
    val clashing = tableau.filter(trow => trow.row(pivot.colIndex) != 0)
//    if (clashing.isEmpty) {
//      (pivot, tableaus)
//    } else {
      val tRowOut = clashing.tail.foldLeft((clashing.first.row(0) / clashing.first.row(pivot.colIndex), clashing.first))((result, trow) => {
        val ratio = trow.row(0) / trow.row(pivot.colIndex)
        if (ratio > result._1) (ratio, trow) else result
      })._2

      // changing the clashed row
      val oldFactor = tRowOut.row(pivot.colIndex)
      val newClashRow = (0 until tRowOut.row.length).map(i => 
        if ( i == tRowOut.basis.colIndex) 
          1 / oldFactor
        else if (i == pivot.colIndex)
          new Rational(0)
        else
          tRowOut.row(i) / oldFactor * (-1)
      ).toList
    
      // update all rows
      val tRowOutIndex = tableau.indexOf(tRowOut)
      val newTableau = (0 until tableau.length).map(j => {
        if (j == tRowOutIndex) TableauRow(newClashRow, pivot) else {
          val trow = tableau(j)
          if (trow.row(pivot.colIndex) != 0) {
            val oldFactor = trow.row(pivot.colIndex)
            val newRow = (0 until trow.row.length).map(i => 
              if (i == pivot.colIndex) new Rational(0) else trow.row(i) + oldFactor * newClashRow(i)).toList
            TableauRow(newRow, trow.basis)
          } else trow
        } 
      }).toList
      (tRowOut.basis, tableaus.updated(pivot.tableauIndex, newTableau))
//    }
  }

  /**
   * Normalises a solution by setting values of slack variables to zero and by division through the sum of values (it sums up to one, subsequently).
   * It also sorts the solution by index of the variables.
   */
  def normalise(tableau:List[List[TableauRow]]) = {
    
//    val solution1 = tableau(0).map(trow => if (trow.basis.isSlack) (trow.basis, new Rational(0)) else (trow.basis, trow.row(0))).toList
    val solution1 = tableau(0).map(trow => (trow.basis, trow.row(0))).toList
    val solution2 = tableau(1).map(trow => (trow.basis, trow.row(0))).toList
    if ((solution1 ++ solution2).exists(p => p._2 < 0)) { // no transformation if any variable is negative (will be removed later)
      (solution1 ++ solution2)
    } else {
      val sum1 = solution1.foldLeft(new Rational(0))((sum, el) => if (el._1.isSlack) sum  else sum + el._2)
      val sum2 = solution2.foldLeft(new Rational(0))((sum, el) => if (el._1.isSlack) sum  else sum + el._2)
      val both = (solution1.map(pair => (pair._1, if (sum1 != 0) pair._2 / sum1 else pair._2)) ++ 
       solution2.map(pair => (pair._1, if (sum2 != 0) pair._2 / sum2 else pair._2))
      ).sort((s,t) => s._1.index < t._1.index)
      both.map(pair => (pair._1, if (pair._1.isSlack) new Rational(0) else pair._2))
    }
  }
}