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
  def printSolutions(game: TwoPersonsGame, withExpectations: Boolean, optimalInd:String =  "P"): String = {
    
    val expectations = game.solutions.map(row => {
      val (x, y) = row.map(_._2).splitAt(game.pm1.nrow)
      val ex = (x zip game.pm1.getExpectations(y, false)).foldLeft(new Rational(0))((r, pair) => r + pair._1 * pair._2)
      val ey = (y zip game.pm2.getExpectations(x, true)).foldLeft(new Rational(0))((r, pair) => r + pair._1 * pair._2)
      (ex, ey)
    })
    
    val pureExpectations = game.pm1.content zip game.pm2.content
    val mpf = new MaximalPairFinder(expectations ++ pureExpectations)
    
    val prettySol = game.solutions.map(_.map(_._2.pretty))
    val prettyExp = expectations.map(p => (p._1.pretty, p._2.pretty))
    
    val maxLengthSol = prettySol.foldLeft(0)((size, p) => Math.max(size, p.foldLeft(0)((s, el) => Math.max(s, el.size))))
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
    manager.addPrimitive("matrix-from-row-list", new GetMatrixFromRowList)
    manager.addPrimitive("matrix-transpose", new GetMatrixTranspose)
    manager.addPrimitive("matrix-as-pretty-strings", new GetMatrixAsPrettyStrings)
    
    manager.addPrimitive("two-persons-game", new GetTwoPersonsGame)
    manager.addPrimitive("two-persons-gamut-game", new GetTwoPersonsGamutGame)
    manager.addPrimitive("game-matrix", new GetGameMatrix)
    manager.addPrimitive("game-solutions", new GetGameSolutions)
    manager.addPrimitive("game-pure-optima", new GetGamePureOptima)
    manager.addPrimitive("get-solutions-string", new GetSolutionsString)
    manager.addPrimitive("get-solutions-string-with-expect", new GetSolutionsStringWithExpectations)
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
  
  def getRowList(transpose: Boolean): List[List[Rational]] = if (transpose) {
    val rm = content.foldLeft(((0 until ncol).map(i =>(i -> List[Rational]())).toMap, 0))((result, entry) => {
      (result._1.updated(result._2, entry :: result._1(result._2)) , (result._2 + 1) % ncol)
    })._1
    (0 until ncol).map(i => rm(i).reverse).toList
  } else {
    (0 until nrow).foldLeft((content, List[List[Rational]]()))((pair, _) => {
      val (row, rest) = pair._1.splitAt(ncol)
      (rest, row :: pair._2)
    })._2.reverse
  }
  
  /**
   * needed by the LemkeHowsonSolver
   */
  def getTableau(basis: List[Variable], transpose: Boolean): List[TableauRow] = {
    val rowList = getRowList(transpose)
    (rowList zip basis).map(pair => 
      TableauRow(List[Rational](1) ++ (0 until rowList.length).map(_ => new Rational(0)) ++ pair._1.map(e => e * -1), pair._2)
    ).toList
  }
  
  /**
   * calculates the expected payoff of each row given the probabilities x of the columns
   */
  def getExpectations(x: List[Rational], transpose: Boolean) = {
    val rowList = getRowList(transpose)
    rowList.map(row => (row zip x).foldLeft(new Rational(0))((r, pair) => {
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
  val solutions = lhs.run
  
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
    new PayoffMatrix(pm.getRowList(true).flatten, pm.ncol, pm.nrow)
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


class GetTwoPersonsGame extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType, WildcardType), WildcardType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    
    val pm1 = args(0).get.asInstanceOf[PayoffMatrix]
    val pm2 = args(1).get.asInstanceOf[PayoffMatrix]

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

class GetGameSolutions extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType), ListType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    val game = args(0).get.asInstanceOf[TwoPersonsGame]
    game.solutions.toLogoList
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

class GetSolutionsString extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType), ListType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    val game = args(0).get.asInstanceOf[TwoPersonsGame]
    GamesExtension.printSolutions(game, false).toLogoObject
  }
}

class GetSolutionsStringWithExpectations extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType), ListType)
  
  private def lines(n: Int) = (0 until n).foldLeft("")((str, el) => str + "-")
  private def spaces(n: Int) = (0 until n).foldLeft("")((str, el) => str + " ")
  
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
    
    val pureSolutionsIdx = game.solutions.foldLeft(List[Int]())((result, row) => {
      val idx = row.foldLeft(List[Int]())((t, el) => if (el._2 == 1) el._1.index :: t else t)
      if (idx.length == 2)
        (idx.first - game.pm1.nrow + 1 + idx.last * game.pm1.ncol ) :: result
      else
        result
    })
    
    val maxLengthNr = game.pm1.content.length.toString().length()
    val maxLengthEntries = (game.pm1.content ++ game.pm2.content).map(_.floor).max.toString().length()
    
    val stringList = (1 to game.pm1.content.length).map(i => { 
      val (x,y) = pureExpectations(i - 1)
      spaces(maxLengthNr + 1 - i.toString.length()) + i + ": (" + 
      spaces(maxLengthEntries - x.floor.toString.length()) + x.floor + "," + 
      spaces(maxLengthEntries - y.floor.toString().length()) + y.floor + ") " + 
      (if (isMaxima(i - 1)) "P" else " ") + (if (pureSolutionsIdx.contains(i)) "N" else " ")
    })
    
    val result = stringList.foldLeft(("", 1))((r, el) => if (r._2 == game.pm1.ncol) (r._1 + "|" + el + "|\n", 1 ) else (r._1 + "|" + el , r._2 + 1))._1
    result.toLogoObject
  }
  
}
