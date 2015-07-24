package de.games

import java.util.{ List => JList }

import org.nlogo.api._
import org.nlogo.api.Syntax._
import org.nlogo.api.ScalaConversions._

import de.util.Rational._

class GamesExtension extends DefaultClassManager {
  
  override def load(manager: PrimitiveManager) {
    manager.addPrimitive("from-row-list", new FromRowList)
//    manager.addPrimitive("from-column-list", new FromColumnList)
    manager.addPrimitive("pm-transpose", new PMTranspose)
    manager.addPrimitive("pm-get-row", new PMGetRow)
    manager.addPrimitive("get-solutions", new GetSolutions)
    manager.addPrimitive("get-solutions-string", new GetSolutionsString)
  }
  
  override def additionalJars: JList[String] = {
    val list : java.util.List[String] =  new java.util.ArrayList[String]
//    list.add("config-1.0.2.jar")
    list
  }
  
  override def clearAll() {
    
  }

}


class PayoffMatrix(val content: List[Rational], val nrow:Int, val ncol:Int) extends ExtensionObject {
  require(content.length == nrow * ncol)
  
  def dump(readable: Boolean, exporting: Boolean, reference: Boolean): String = toString
  def getExtensionName: String = "games"
  def getNLTypeName: String = "PayoffMatrix"
  def recursivelyEqual(obj: AnyRef): Boolean = equals(obj)
  
  def getRow(row:Int) = content.drop(row * ncol).take(ncol)
  
  val pretty = content.map(_.prettyDouble(1))
  val maxLength = pretty.foldLeft(0)((size, p) => Math.max(size, p.size))
  
  def getRowString(row:Int) = pretty.drop(row * ncol).take(ncol).map(p => 
    (0 until (maxLength - p.size)).foldLeft("")((str, el) => str + " ") + p)
  
  def getTableau(basis: List[Variable], transpose: Boolean): List[TableauRow] = {
    
    val rowList = if (transpose) {
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
    
    (rowList zip basis).map(pair => 
      TableauRow(List[Rational](1) ++ (0 until rowList.length).map(_ => new Rational(0)) ++ pair._1.map(e => e * -1), pair._2)
    ).toList
  }
  
}

class FromRowList extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](ListType), WildcardType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    
    val rowList = args(0).getList.map(row => 
      row.asInstanceOf[LogoList].map(e => new Rational(e.asInstanceOf[Double])).toList).toList
    val content = rowList.flatMap(f => f)
    
    new PayoffMatrix(content, rowList.length, rowList.first.length)
    
  }
  
}


class PMTranspose extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType), WildcardType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    
    val pm = args(0).get.asInstanceOf[PayoffMatrix]
    
    val nrow = pm.ncol
    val rowMap = pm.content.foldLeft(((0 until nrow).map(i =>(i -> List[Rational]())).toMap, 0))((result, entry) => {
      (result._1.updated(result._2, entry :: result._1(result._2)) , (result._2 + 1) % nrow)
    })._1
    val content = (0 until nrow).flatMap(i => rowMap(i).reverse).toList
    
    new PayoffMatrix(content, nrow, pm.nrow)
    
  }
  
}

class PMGetRow extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType, NumberType), ListType)
  
  def report(args: Array[Argument], c: Context): AnyRef = {
    val pm = args(0).get.asInstanceOf[PayoffMatrix]
    pm.getRowString(args(1).getIntValue).toLogoList
  }
  
}

class GetSolutions extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType, WildcardType), ListType)
  
  private def prettyPrint(normSol: List[(Variable, Rational)]) = {
      normSol.foldLeft("")((str, el) => {
        val p = el._2.pretty
        str + " | " + el._1.index + ": " + p + (0 until (6 - p.size)).foldLeft("")((str, el) => str + " ")
      })
  }
  
  def report(args: Array[Argument], c: Context): AnyRef = {
  
    val lhs = new LemkeHowsonSolver(args(0).get.asInstanceOf[PayoffMatrix], args(1).get.asInstanceOf[PayoffMatrix])
    val solutions = lhs.run
    val pretty = solutions.map(_.map(_._2.pretty))
    val maxLength = pretty.foldLeft(0)((size, p) => Math.max(size, p.foldLeft(0)((s, el) => Math.max(s, el.size))))
    val result = pretty.map(list => {
      list.map(p => {
        (0 until (maxLength - p.size)).foldLeft("")((str, el) => str + " ") + p    
      })
    })
    
    result.toLogoList
  }
  
}

class GetSolutionsString extends DefaultReporter {
  
  override def getAgentClassString = "O"
  override def getSyntax = reporterSyntax(Array[Int](WildcardType, WildcardType), ListType)
  
  private def lines(n: Int) = (0 until n).foldLeft("")((str, el) => str + "-")
  private def spaces(n: Int) = (0 until n).foldLeft("")((str, el) => str + " ")
  
  def report(args: Array[Argument], c: Context): AnyRef = {
  
    val pm1 = args(0).get.asInstanceOf[PayoffMatrix]
    val pm2 = args(1).get.asInstanceOf[PayoffMatrix]
    
    val lhs = new LemkeHowsonSolver(pm1, pm2)
    val solutions = lhs.run
    val pretty = solutions.map(_.map(_._2.pretty))
    val maxLength = pretty.foldLeft(0)((size, p) => Math.max(size, p.foldLeft(0)((s, el) => Math.max(s, el.size))))
    
    val header = (1 to pm1.nrow).foldLeft("")((a,b) => a + spaces(maxLength) + "x" + b ) +  
                 (1 to pm1.ncol).foldLeft("")((a,b) => a + spaces(maxLength) + "y" + b ) + "\n" + 
                 (1 to pm1.nrow + pm1.ncol).foldLeft("")((a,b) => a + lines(maxLength + 2) ) + "\n"
    
    val result = header + pretty.foldLeft("")((st, row) => st + row.foldLeft("")((a,b) => a + spaces(maxLength - b.size + 2) + b ) + "\n")
    
    result.toLogoObject
  }
  
}
