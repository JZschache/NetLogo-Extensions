package de.games

import java.util.{ List => JList }

import org.nlogo.api._
import org.nlogo.api.Syntax._
import org.nlogo.api.ScalaConversions._

import de.util.Rational._

object GamesExtension {
  
  private def lines(n: Int) = (0 until n).foldLeft("")((str, el) => str + "-")
  private def spaces(n: Int) = (0 until n).foldLeft("")((str, el) => str + " ")
  
  /**
   * prints the solutions of a game in a table with header
   * 
   * withExpectations includes the expected values and indicator of optimality (optimalInd)
   * 
   */
  def printSolutions(game: TwoPersonsGame, withExpectations: Boolean, optimalInd:String =  "O"): String = {
    
    val expectations = game.mixedSolutions.map(row => {
      val (x, y) = row.map(_._2).splitAt(game.pm1.nrow)
      val ex = (x zip game.pm1.getExpectations(y, false)).foldLeft(new Rational(0))((r, pair) => r + pair._1 * pair._2)
      val ey = (y zip game.pm2.getExpectations(x, true)).foldLeft(new Rational(0))((r, pair) => r + pair._1 * pair._2)
      (ex, ey)
    })
    
    val pureExpectations = game.pm1.content zip game.pm2.content
    val mpf = new MaximalPairFinder(expectations ++ pureExpectations)
    
    val prettySol = game.mixedSolutions.map(_.map(_._2.pretty))
    val prettyExp = expectations.map(p => (p._1.pretty, p._2.pretty))
    
    val maxLengthSol = Math.max(1, prettySol.foldLeft(0)((size, p) => Math.max(size, p.foldLeft(0)((s, el) => Math.max(s, el.size)))))
    val maxLength = if (withExpectations) 
                      Math.max(maxLengthSol, prettyExp.foldLeft(0)((size, p) => Math.max(size, Math.max(p._1.size, p._2.size))))
                    else
                      maxLengthSol
    
    val colLength = if (withExpectations) game.pm1.nrow + game.pm1.ncol + 5 else game.pm1.nrow + game.pm1.ncol
    val header = (1 to game.pm1.nrow).foldLeft("")((a,b) => a + spaces(maxLength) + "x" + b ) +  
                 (1 to game.pm1.ncol).foldLeft("")((a,b) => a + spaces(maxLength) + "y" + b ) + 
                 (if (withExpectations) 
                   "  |" + spaces(maxLength) + "Ex" + spaces(maxLength) + "Ey" + "  |" + spaces(maxLength)  + "mx" 
                 else 
                   "") +  "\n" + 
                 (1 to colLength).foldLeft("")((a,b) => a + lines(maxLength + 2) ) + "\n"

    if (withExpectations) {
      header + (prettySol, prettyExp, mpf.maximaIndices.take(prettyExp.length)).zipped.foldLeft("")((st, row) => st + 
        row._1.foldLeft("")((a,b) => a + spaces(maxLength - b.size + 2) + b ) +
        "  |" + spaces(maxLength - row._2._1.size + 2) + row._2._1 + spaces(maxLength - row._2._2.size + 2) + row._2._2 + "  |" +
             spaces(maxLength + 1) + (if (row._3) optimalInd else " ") + "\n")
    } else {
      header + prettySol.foldLeft("")((st, row) => st + row.foldLeft("")((a,b) => a + spaces(maxLength - b.size + 2) + b ) + "\n")
    }
  }
}

class GamesExtension extends DefaultClassManager {
  
  override def load(manager: PrimitiveManager) {
    // constructor of and reporters on matrices
    manager.addPrimitive("matrix-from-row-list", new GetMatrixFromRowList)
    manager.addPrimitive("matrix-transpose", new GetMatrixTranspose)
    manager.addPrimitive("matrix-as-pretty-strings", new GetMatrixAsPrettyStrings)
    manager.addPrimitive("get-reward",new GetReward)
    //constructors of games
    manager.addPrimitive("two-persons-game", new GetTwoPersonsGame)
    manager.addPrimitive("two-persons-gamut-game", new GetTwoPersonsGamutGame)
    // reporters on game objects
    manager.addPrimitive("game-matrix", new GetGameMatrix)
    manager.addPrimitive("game-pure-solutions", new GetGamePureSolutions)
    manager.addPrimitive("game-pure-optima", new GetGamePureOptima)
    manager.addPrimitive("get-solutions-string", new GetSolutionsString)
//    manager.addPrimitive("get-solutions-string-with-expect", new GetSolutionsStringWithExpectations)
    manager.addPrimitive("get-fields-string", new GetFieldsString)
  }
  
  override def additionalJars: JList[String] = {
    val list : java.util.List[String] =  new java.util.ArrayList[String]
//    list.add("config-1.0.2.jar")
    list
  }
  
  override def clearAll() {
    
  }

}

//////////////////////
// ExtensionObjects //
//////////////////////

class PayoffMatrix(val content: List[Rational], val nrow:Int, val ncol:Int) extends ExtensionObject {
  require(content.length == nrow * ncol)
  
  def dump(readable: Boolean, exporting: Boolean, reference: Boolean): String = toString
  def getExtensionName: String = "games"
  def getNLTypeName: String = "PayoffMatrix"
  def recursivelyEqual(obj: AnyRef): Boolean = equals(obj)
    
  def getRow(row:Int): List[Rational] = content.drop(row * ncol).take(ncol)
  
  def getRowList: List[List[Rational]] = {
    (0 until nrow).foldLeft((content, List[List[Rational]]()))((pair, _) => {
      val (row, rest) = pair._1.splitAt(ncol)
      (rest, row :: pair._2)
    })._2.reverse
  }
  
  def getColumnList: List[List[Rational]] = { 
    val rm = content.foldLeft(((0 until ncol).map(i =>(i -> List[Rational]())).toMap, 0))((result, entry) => {
      (result._1.updated(result._2, entry :: result._1(result._2)) , (result._2 + 1) % ncol)
    })._1
    (0 until ncol).map(i => rm(i).reverse).toList
  }
  
  
  private val array = getRowList.map(list => list.toArray).toArray
  
  def getReward(rowId:Int, colId:Int) = array(rowId)(colId)
  
  /**
   * needed by the LemkeHowsonSolver
   */
  def getTableau(basis: List[Variable], transpose: Boolean): List[TableauRow] = {
    val list = if (transpose) getColumnList else getRowList
    val length = list.length
    // for lexicographic comparison ( if ever needed again)
//    val identity = (0 until length).map(i => (0 until length).map(j => if (j == i) new Rational(1) else new Rational(0)))
//    (rowList, identity, basis).zip.map(triple => 
//      TableauRow(List[Rational](1) ++ (0 until length).map(_ => new Rational(0)) ++ 
//                 triple._1.map(e => e * -1) ++ triple._2, triple._3)
//    ).toList
    (list, basis).zip.map(pair => 
      TableauRow(List[Rational](1) ++ (0 until length).map(_ => new Rational(0)) ++ 
                 pair._1.map(e => e * -1), pair._2)
    ).toList
  }
  
  /**
   * calculates the expected payoff of each row given the probabilities x of the columns
   */
  def getExpectations(x: List[Rational], transpose: Boolean) = {
    val list = if (transpose) getColumnList else getRowList
    list.map(row => (row zip x).foldLeft(new Rational(0))((r, pair) => {
      r + pair._1 * pair._2
    }))
  }

  // the entries are pretty if they are all strings of the the same length
  // note that entries are assumed to be integers
  private val pretty = content.map(_.floor)
  private val maxLength = pretty.max.toString.size
  private def getPrettyRow(row:Int) = pretty.drop(row * ncol).take(ncol).map(p => 
    (0 until (maxLength - p.toString.size)).foldLeft("")((str, el) => str + " ") + p)
  def getPrettyRows = (0 until nrow).map(i => getPrettyRow(i)).toList
  
}


class TwoPersonsGame(val pm1: PayoffMatrix, val pm2: PayoffMatrix) extends ExtensionObject {
  
  def dump(readable: Boolean, exporting: Boolean, reference: Boolean): String = toString
  def getExtensionName: String = "games"
  def getNLTypeName: String = "TwoPersonGame"
  def recursivelyEqual(obj: AnyRef): Boolean = equals(obj)
  
  
  val lhs = new LemkeHowsonSolver(pm1, pm2)
  private val sampleSolutions = lhs.run
  
  val mixedSolutions = sampleSolutions.filter(sol => {
    2 != sol.foldLeft(0)((t, el) => if (el._2 == 1) 1 + t else t)
  })
  
  // calculate all pure solutions separately
  private val isMaxima1 = pm1.getColumnList.map(column => {
    val maxEntry = column.reduce((a,b) => if (a > b) a else b)
    column.map(_ == maxEntry)
  })
  private val isMaxima1Trans = isMaxima1.foldLeft((0 until pm1.nrow).map(i => List[Boolean]()))((result, entry) => {
    (result zip entry).map(pair => pair._2 :: pair._1)
  }).map(row => row.reverse).toList
  private val isMaxima2 = pm2.getRowList.map(row => {
    val maxEntry = row.reduce((a,b) => if (a > b) a else b)
    row.map(_ == maxEntry)
  })
  
  val isPureSolution = (isMaxima1Trans zip isMaxima2).map(pair => {
    (pair._1 zip pair._2).map(p => p._1 & p._2)
  })
  
 
  
}


//////////////////////////
// Reporters / Commands //
//////////////////////////

class GetMatrixFromRowList extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](ListType), WildcardType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    
    val rowList = args(0).getList.map(row => 
      row.asInstanceOf[LogoList].map(e => new Rational(e.asInstanceOf[Double])).toList).toList
    val content = rowList.flatMap(f => f)
    
    new PayoffMatrix(content, rowList.length, rowList.first.length)
  }
}

class GetMatrixTranspose extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType), WildcardType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    val pm = args(0).get.asInstanceOf[PayoffMatrix]
    if (pm.ncol != pm.nrow){
      throw new org.nlogo.api.ExtensionException("A game matrix must quadric for its transpostion to work.")
    }
    new PayoffMatrix(pm.getColumnList.flatten, pm.ncol, pm.nrow)
  }
}

class GetMatrixAsPrettyStrings extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType), ListType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    val pm = args(0).get.asInstanceOf[PayoffMatrix]
    pm.getPrettyRows.toLogoList
  }
}

class GetReward extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType, NumberType, NumberType), NumberType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    val pm = args(0).get.asInstanceOf[PayoffMatrix]
    pm.getReward(args(1).getIntValue, args(2).getIntValue).getDouble.toLogoObject
  }
}


class GetTwoPersonsGame extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType, WildcardType), WildcardType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    
    val pm1 = args(0).get.asInstanceOf[PayoffMatrix]
    val pm2 = args(1).get.asInstanceOf[PayoffMatrix]

    if (pm1.ncol != pm2.ncol || pm1.nrow != pm2.nrow){
      throw new org.nlogo.api.ExtensionException("Input matrices must match in row and column length respectively.")
    }
    
    new TwoPersonsGame(pm1, pm2)
  }
}

class GetTwoPersonsGamutGame extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](StringType, NumberType, NumberType), WildcardType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    
    val name = args(0).getString
    val nAlt1 = args(1).getIntValue
    val nAlt2 = args(2).getIntValue

    val g = TwoPersonGamutGame(name, Array(nAlt1, nAlt2))
    
    val realNAlt1 = g.getNumActions(0)
    val realNAlt2 = g.getNumActions(1)
    
    val (c1, c2) = (for {i <- 1 to realNAlt1; j <- 1 to realNAlt2} 
                    yield (g.getOutputPayoff(Array(i,j), 0).toInt, g.getOutputPayoff(Array(i,j), 1).toInt)).unzip

    val pm1 = new PayoffMatrix(c1.map(i => new Rational(i)).toList, realNAlt1, realNAlt2)
    val pm2 = new PayoffMatrix(c2.map(i => new Rational(i)).toList, realNAlt1, realNAlt2)
    new TwoPersonsGame(pm1, pm2)               
  }
}

class GetGameMatrix extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType, NumberType), WildcardType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    
    val game = args(0).get.asInstanceOf[TwoPersonsGame]
    val idx = args(1).getIntValue
    if (idx == 1) game.pm1 else game.pm2
  }
}

class GetGamePureSolutions extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType), ListType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    val game = args(0).get.asInstanceOf[TwoPersonsGame]
    game.isPureSolution.flatten.toLogoList
  }
}

class GetGamePureOptima extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType), ListType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    val game = args(0).get.asInstanceOf[TwoPersonsGame]
    val pureExpectations = game.pm1.content zip game.pm2.content
    val mpf = new MaximalPairFinder(pureExpectations)
    val isMaxima = mpf.maximaIndices
    isMaxima.toLogoList
  }
}

//class GetSolutionsString extends DefaultReporter {
//  
//  override def getAgentClassString = "O"
//  override def getSyntax = reporterSyntax(Array[Int](WildcardType), ListType)
//  
//  def report(args: Array[Argument], c: Context): AnyRef = {
//    val game = args(0).get.asInstanceOf[TwoPersonsGame]
//    GamesExtension.printSolutions(game, false).toLogoObject
//  }
//}

class GetSolutionsString extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType), ListType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    val game = args(0).get.asInstanceOf[TwoPersonsGame]
    GamesExtension.printSolutions(game, true).toLogoObject
  }
}

class GetFieldsString extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType), ListType)
  
  private def spaces(n: Int) = (0 until n).foldLeft("")((str, el) => str + " ")
  
  def report(args: Array[Argument], c: Context): AnyRef = {
  
    val game = args(0).get.asInstanceOf[TwoPersonsGame]
    
    val pureExpectations = game.pm1.content zip game.pm2.content
    val mpf = new MaximalPairFinder(pureExpectations)
    val isMaxima = mpf.maximaIndices
    val isSolution = game.isPureSolution.flatten
    
    val maxLengthNr = game.pm1.content.length.toString().length()
    val maxLengthEntries = (game.pm1.content ++ game.pm2.content).map(_.floor).max.toString().length()
    
    val stringList = (1 to game.pm1.content.length).map(i => { 
      val (x,y) = pureExpectations(i - 1)
      spaces(maxLengthNr + 1 - i.toString.length()) + i + ": (" + 
      spaces(maxLengthEntries - x.floor.toString.length()) + x.floor + "," + 
      spaces(maxLengthEntries - y.floor.toString().length()) + y.floor + ") " + 
      (if (isMaxima(i - 1)) "O" else " ") + (if (isSolution(i - 1)) "N" else " ")
    })
    
    val result = stringList.foldLeft(("", 1))((r, el) => if (r._2 == game.pm1.ncol) (r._1 + "|" + el + "|\n", 1 ) else (r._1 + "|" + el , r._2 + 1))._1
    result.toLogoObject
  }
  
}
