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
case class Tableau(_1:List[TableauRow], _2:List[TableauRow]) {
  
  def apply(i: Int) = if (i == 0) _1 else if (i == 1) _2 else Nil
  
  def updated(i: Int, newPart: List[TableauRow]) = if (i == 0) Tableau(newPart, _2) else if (i == 1) Tableau(_1, newPart) else this
  
  def together = _1 ++ _2
}

class LemkeHowsonSolver(p1: PayoffMatrix, p2: PayoffMatrix ) {
  
  // alter matrices to be nonnegative and to have no zero columns / rows
  val entries = p1.content ++ p2.content
  val minEntry = entries.tail.foldLeft(entries.first)((min, entry) => if (entry < min) entry else min)
  val summand = 1 + (if (minEntry < 0) minEntry * -1 else minEntry)
  val pm1 = new PayoffMatrix(p1.content.map(_ + summand), p1.nrow, p1.ncol)
  val pm2 = new PayoffMatrix(p2.content.map(_ + summand), p2.nrow, p2.ncol)
  
  // the variables (as pairs of slack and non-slack variables)
  val variables1 = (0 until pm1.nrow).map(i => (i -> (Variable(i, true, 1 + i, 0), Variable(i, false, 1 + pm1.ncol + i, 1)))).toMap
  val variables2 = (0 until pm2.ncol).map(i => (pm1.nrow + i -> (Variable(pm1.nrow + i, true, 1 + i, 1), Variable(pm1.nrow + i, false, 1 + pm2.nrow + i, 0)))).toMap
  val variables = variables1 ++ variables2
  
  // the equation systems with slack variables as basis (called 'tableau' Codenotti et al )
  val tab1 = pm1.getTableau((0 until pm1.nrow).map(i => variables1(i)._1).toList, false)
  val tab2 = pm2.getTableau((pm1.nrow until pm1.nrow + pm2.ncol).map(i => variables2(i)._1).toList, true)
  
  /**
   * the algorithm is run for each non-slack variable and, 
   * from this point, again for every variable that is not in the solution
   * 
   * it returns a list of solutions. 
   * a solution is a list of pairs, each pair containing a variable and a rational number (the probability)
   */
  def run() = {
    
    val result = variables.values.flatMap(v => {
      
      val solvedTableaus1 = solve(v._2, Tableau(tab1, tab2), v._2, 0)
      
      val normSolutions = solvedTableaus1.map(st1 => normalise(st1))
      
      normSolutions ++ solvedTableaus1.flatMap(st =>
        st.together.flatMap(j => {
          val basis = j.basis
          val solvedTableau2 = if (basis.isSlack) {
            solve(variables(basis.index)._2, st, variables(basis.index)._2, 0)
          } else {
            solve(variables(basis.index)._1, st, variables(basis.index)._1, 0)
          }
          solvedTableau2.map(st2 => normalise(st2))
        }))
      }).toList

     // filter the all zero solutions or solutions with a negative entry
    val filtered = result.filter(el => !(el.forall(_._2 == 0) || el.exists(_._2 < 0)))
        
    // return only distinct solutions
    filtered.foldLeft(List[List[(Variable, Rational)]]())((result, el) => {
      if (result.exists(p => (p zip el).forall(pair => pair._1._2 == pair._2._2)))
        result
      else
        el :: result
    })
  }
  
  def prettyPrint(normSol: List[(Variable, Rational)]) = {
    normSol.foldLeft("")((str, el) => {
      val p = el._2.pretty
      str + " | " + el._1.index + ": " + p + (0 until (6 - p.size)).foldLeft("")((str, el) => str + " ")
    })
  }

  
  def solve (pivot: Variable, t: Tableau, startPivot: Variable, count: Int): List[Tableau] = {
    
    val tPart = t(pivot.tableauIndex)
    
    // for lexicographic comparison ( if ever needed again)
//    val firstPertubationIndex = tPart.first.row.length - tPart.length
//    val lastPertubationIndex = tPart.first.row.length - 1
//    def ratioVector(trow: TableauRow) = {
//      List(trow.row(0) / trow.row(pivot.colIndex)) ++ (firstPertubationIndex to lastPertubationIndex).map(i => trow.row(i) / trow.row(pivot.colIndex))
//    }
//    def compare(first: List[Rational], second: List[Rational]) = {
//      val unequal = (first zip second).find(p => p._1 != p._2)
//      if (unequal.isEmpty) 0
//      else {
//        if (unequal.get._1 > unequal.get._2) 1
//        else -1
//      }
//    }
    
    
    val clashing = tPart.filter(trow => trow.row(pivot.colIndex) < 0)
    if (clashing.isEmpty || count == 10) // break if no clashes or if recursion too deep
      Nil
    else {
      val minRatioRows = clashing.tail.foldLeft((clashing.first.row(0) / clashing.first.row(pivot.colIndex) , List(clashing.first)))((result, trow) => {
        val ratio = trow.row(0) / trow.row(pivot.colIndex)
        if (ratio > result._1) (ratio, List(trow)) 
        else if (ratio == result._1) (ratio, trow :: result._2) 
        else result
      })._2

      minRatioRows.map(row => step(pivot, row, tPart)).flatMap(pair => {
      
        val (varOut, newTablPart) = pair
        val newTableau = t.updated(pivot.tableauIndex, newTablPart)
        val newPivot = if (varOut.isSlack) variables(varOut.index)._2 else variables(varOut.index)._1
      
        if (newPivot.index != startPivot.index) {
          solve(newPivot, newTableau, startPivot, count + 1)
        } else {
          List(newTableau)
        }
      })
    }
  }
  
  /**
   * A step of the algorithm.
   * 
   * It returns the variable that has left the basis and the changed part of the new tableaus
   */
  def step(pivot: Variable, tRowOut: TableauRow, tableau: List[TableauRow]) = {
  
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
    (tRowOut.basis, newTableau)
  }

  /**
   * Normalises a solution by setting values of slack variables to zero and by division through the sum of values (it sums up to one, subsequently).
   * It also sorts the solution by index of the variables.
   */
  def normalise(tableau: Tableau) = {
    
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