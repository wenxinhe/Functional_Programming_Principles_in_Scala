package idealized.scala

object _true extends _Boolean {
  def ifThenElse(t: => _Boolean, e: => _Boolean) = t
}

object _false extends _Boolean {
  def ifThenElse(t: => _Boolean, e: => _Boolean) = e
}

abstract class _Boolean {
  def ifThenElse(t: => _Boolean, e: => _Boolean): _Boolean

  def &&(x: => _Boolean): _Boolean = ifThenElse(x, _false)

  def ||(x: => _Boolean): _Boolean = ifThenElse(_true, x)

  def unary_! : _Boolean = ifThenElse(_false, _true)

  def ==(x: _Boolean): _Boolean = ifThenElse(x, x.unary_!)

  def !=(x: _Boolean): _Boolean = ifThenElse(x.unary_!, x)

  def <(x: _Boolean): _Boolean = ifThenElse(_false, x)
}


object Zero extends Nat {
  def isZero: Boolean = true

  def predecessor: Nat = throw new Error("0.predecessor")

  def +(that: Nat): Nat = that

  def -(that: Nat): Nat = if (that.isZero) this else throw new Error("negative number")
}


class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false

  def predecessor: Nat = n

  def +(that: Nat): Nat = new Succ(n + that)

  def -(that: Nat): Nat = n - that.predecessor
}

abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat

  def -(that: Nat): Nat
}

