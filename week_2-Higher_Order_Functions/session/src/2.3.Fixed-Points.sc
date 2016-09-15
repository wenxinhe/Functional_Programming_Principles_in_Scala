def abs(x: Double) =
  if (x < 0) -x else x

val tolerance = 0.0001
def isCloseEnough(x: Double, y: Double) =
  abs((x - y) / x) / x < tolerance
def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) guess
    else iterate(next)
  }
  iterate(firstGuess)
}

def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2)(1.0)


sqrt(2)