package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions map {case (key, value) => {
      if (isCyclic(key, value(), namedExpressions)) (key, Signal(Double.NaN))
      else (key, Signal(eval(value(), namedExpressions)) )
    }}
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
       case Literal(v: Double) => v
       case Ref(name: String) => eval(getReferenceExpr(name, references), references)
       case Plus(a: Expr, b: Expr) => { eval(a, references) + eval(b, references)}
       case Minus(a: Expr, b: Expr) => { eval(a, references) - eval(b, references)}
       case Times(a: Expr, b: Expr) => { eval(a, references) * eval(b, references)}
       case Divide(a: Expr, b: Expr) => { eval(a, references) / eval(b, references)}
     }
  }

  def isCyclic(ref: String, expr: Expr, references: Map[String, Signal[Expr]]): Boolean = {
    expr match {
      case Literal(v: Double) => false
      case Ref(name: String) => { if (name == ref) true
                                  else isCyclic(ref, getReferenceExpr(name, references), references)}
      case Plus(a: Expr, b: Expr) => {isCyclic(ref, a, references) || isCyclic(ref, b, references)}
      case Minus(a: Expr, b: Expr) => {isCyclic(ref, a, references) || isCyclic(ref, b, references)}
      case Times(a: Expr, b: Expr) => {isCyclic(ref, a, references) || isCyclic(ref, b, references)}
      case Divide(a: Expr, b: Expr) => {isCyclic(ref, a, references) || isCyclic(ref, b, references)}
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
   def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {

    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
