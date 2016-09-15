class Rational(x: Int, y: Int) {

  require(y > 0, "denominator must be positive")

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  //  private val g = gcd(x, y)

  val numer = x / gcd(x, y)
  val denom = y / gcd(x, y)

  // auxiliary constructors
  def this(x: Int) = this(x, 1)

  def + (that: Rational) =
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def * (that: Rational) =
    new Rational(numer * that.numer, denom * that.denom)

  def unary_- = new Rational(-numer, denom)

  def - (that: Rational) = this + -that

  def < (that: Rational) = numer * that.denom < denom * that.numer

  def max(that: Rational) = if (this < that) that else this

  override def toString = numer + "/" + denom
}

val x = new Rational(1, 2)
val y = new Rational(2, 3)
val z = new Rational(5, 7)

x.numer
x.denom
(x + y) * z
-x
x - y
x < y
x max y

