object A {

  def id(x: Int): Int = x

  def cube(x: Int): Int = x * x * x

  def fact(x: Int): Int = if (x == 0) 1 else fact(x - 1)

  def sum(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum(f, a + 1, b)

  def sumInts(a: Int, b: Int) = sum(x => x, a, b)

  def sumCubes(a: Int, b: Int) = sum(x => x * x * x, a, b)

  def sumFactorials(a: Int, b: Int) = sum(fact, a, b)

  // Note that a and b get passed unchanged from sumInts and sumCubes into  sum .
  // Can we be even shorter by getting rid of these parameters?

  // Functions Returning Functions
  def sumF(f: Int => Int): (Int, Int) => Int = {
    def _sumF(a: Int, b: Int): Int =
      if (a > b) 0
      else f(a) + _sumF(a + 1, b)
    _sumF
  }

  def sumIntsF = sumF(id)

  def sumCubesF = sumF(cube)

  def sumFactorialsF = sumF(fact)

  sumCubes(1, 10) + sumFactorials(10, 20)

  // avoid middlemen
  sumF(cube)(1, 10) + sumF(fact)(10, 20)

  sumF(cube) == (sumF(cube))

  // Multiple Parameter Lists
  // the following definition of sum is equivalent to the one with
  // the nested sumF function, but shorter:
  def sum_currying(f: Int => Int)(a: Int, b: Int): Int = if (a > b) 0 else f(a) + sum_currying(f)(a + 1, b)

  sum_currying(cube)(1, 10) == sumF(cube)(1, 10)

  // the type of sum is
  // (Int => Int) => (Int, Int) => Int

  // exercise

  // Write a product function that calculates the product of the values of
  // a function for the points on a given interval.
  def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1 else f(a) * product(f)(a + 1, b)

  product(x => x * x)(2, 3)

  // Write factorial in terms of product .
  def factorial(n: Int) = product(x => x)(1, n)

  factorial(4)

  // a more general function, which generalizes both sum and product
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, unit: Int)
               (a: Int, b: Int): Int =
  if (a > b) unit else combine(f(a), mapReduce(f, combine, unit)(a + 1, b))

  def sum2(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x, y) => x + y, 0)(a, b)

  def product2(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x, y) => x * y, 1)(a, b)

  sum2(cube)(2, 3)
  product2(cube)(2, 3)

}

