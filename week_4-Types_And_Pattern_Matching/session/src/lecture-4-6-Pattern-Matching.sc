object Expr_4 {

  trait Expr

  case class Number(n: Int) extends Expr

  case class Sum(e1: Expr, e2: Expr) extends Expr

  case class Var(x: String) extends Expr

  case class Prod(e1: Expr, e2: Expr) extends Expr

  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(e1, e2) => eval(e1) + eval(e2)
  }

  def show(e: Expr): String = {
    def show(e: Expr, b: Boolean): String =
      e match {
        case Number(n) => n.toString
        case Var(name) => name.toString
        case Sum(e1, e2) =>
          if (b) "(" + show(e1, false) + " + " + show(e2, false) + ")"
          else show(e1, false) + " + " + show(e2, false)
        case Prod(e1, e2) => show(e1, true) + " * " + show(e2, true)
      }

    show(e, false)
  }

}

import Expr_4._

eval(Sum(Number(1), Number(22)))

show(Sum(Number(1), Number(22)))

show(Sum(Prod(Number(2), Var("x")), Var("y")))

show(Prod(Sum(Number(2), Var("x")), Var("y")))

// (2 + x) * 1 * 2 * (1 + 2)
show(Prod(Sum(Number(2), Var("x")), Prod(Prod(Number(1), Number(2)), Sum(Number(1), Number(2)))))