import scala.annotation.tailrec

object A {

  def id(x: Int): Int = x

  def cube(x: Int): Int = x * x * x

  def fact(x: Int): Int = if (x == 0) 1 else fact(x - 1)

  def sumInts(a: Int, b: Int): Int =
    if (a > b) 0 else a + sumInts(a + 1, b)

  def sumCubes(a: Int, b: Int): Int =
    if (a > b) 0 else cube(a) + sumCubes(a + 1, b)

  def sumFactorials(a: Int, b: Int): Int =
    if (a > b) 0 else fact(a) + sumFactorials(a + 1, b)

  ///////////////////////////

  // higher-order function
  def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f, a + 1, b)

  sum(x => x, 1, 2)

  def sumInts2(a: Int, b: Int) = sum(id, a, b)

  def sumCubes2(a: Int, b: Int) = sum(cube, a, b)

  def sumFactorials2(a: Int, b: Int) = sum(fact, a, b)

  // Anonymous Functions are Syntactic Sugar
  def sumInts3(a: Int, b: Int) = sum(x => x, a, b)

  def sumCubes3(a: Int, b: Int) = sum(x => x * x * x, a, b)

  // tail-recursive version of sum
  def sum_rec(f: Int => Int)(a: Int, b: Int): Int = {
    @tailrec
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }

  sum_rec(x => x)(1, 2)
}


