package week4

trait Expr

case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Var(s: String) extends Expr

object Expr {
  def eval(e: Expr): Int = e match {
    case Number(_) | Var(_) => _
    case Sum(e1, e2) => eval(e1) + eval(e2)
    case Prod(e1, e2) => eval(e1) * eval(e2)
  }

  def show(e: Expr): String = e match {
    case Number(n) => n.toString
    case Sum(e1, e2) => show(e1) + " + " + show(e2)
    case Prod(e1, e2) =>
      def showProd(expr: Expr): String = expr match {
        case Sum(_, _) => "(" + show(expr) + ")"
        case _ => _
      }
      showProd(e1) + " + " + showProd(e2)
    case Var(s) => s
  }
}