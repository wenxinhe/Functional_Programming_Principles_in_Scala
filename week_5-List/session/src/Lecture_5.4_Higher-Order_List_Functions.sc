def scaleList(xs: List[Double], factor: Double): List[Double] = xs match {
  case Nil => xs
  case y :: ys => y * factor :: scaleList(ys, factor)
}

def scaleList_2(xs: List[Double], factor: Double): List[Double] =
  xs map (x => x * factor)

def squareList(xs: List[Double]): List[Double] =
  xs map (x => x * x)

def posElems(xs: List[Int]): List[Int] =
  xs filter (x => x > 0)

val nums = List(1, -4, 3, 7, -1)
val fruits = List("apple", "pineapple", "orange", "banana")

nums filter (x => x > 0)
nums filterNot (x => x > 0)
nums partition (x => x > 0)

nums takeWhile (x => x > 0)
nums dropWhile (x => x > 0)
nums span (x => x > 0)

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (y => y == x)
    first :: pack(rest)
}

def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs) map (ys => (ys.head, ys.length))
}

val data = List('a', 'a', 'a', 'b', 'c', 'c', 'a')
pack(data)
encode(data)



