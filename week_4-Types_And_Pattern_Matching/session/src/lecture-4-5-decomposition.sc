object Expr_1 {

  trait Expr {
    def isNumber: Boolean

    def isSum: Boolean

    def numValue: Int

    def leftOp: Expr

    def rightOp: Expr
  }

  class Number(n: Int) extends Expr {
    def isNumber: Boolean = true

    def isSum: Boolean = false

    def numValue: Int = n

    def leftOp: Expr = throw new Error("Number.leftOp")

    def rightOp: Expr = throw new Error("Number.rightOp")
  }

  class Sum(e1: Expr, e2: Expr) extends Expr {
    def isNumber: Boolean = false

    def isSum: Boolean = true

    def numValue: Int = throw new Error("Sum.numValue")

    def leftOp: Expr = e1

    def rightOp: Expr = e2
  }

  def eval(e: Expr): Int =
    if (e.isSum) e.numValue
    else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
    else throw new Error("Unknown expression " + e)

}

object Expr_2 {

  trait Expr

  class Number(n: Int) extends Expr {
    def numValue: Int = n
  }

  class Sum(e1: Expr, e2: Expr) extends Expr {
    def leftOp: Expr = e1

    def rightOp: Expr = e2
  }

  def eval(e: Expr): Int =
    if (e.isInstanceOf[Number])
      e.asInstanceOf[Number].numValue
    else if (e.isInstanceOf[Sum])
      eval(e.asInstanceOf[Sum].leftOp) +
        eval(e.asInstanceOf[Sum].rightOp)
    else throw new Error("Unknown expression " + e)
}

object Expr_3 {

  trait Expr {
    def eval: Int
  }

  class Number(n: Int) extends Expr {
    def eval: Int = n
  }

  class Sum(e1: Expr, e2: Expr) extends Expr {
    def eval: Int = e1.eval + e2.eval
  }

  // but how can we add more functions for Expr, for instance def show: String
}

