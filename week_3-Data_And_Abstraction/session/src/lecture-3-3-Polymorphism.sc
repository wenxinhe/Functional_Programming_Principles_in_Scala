// type parameter
trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tails: List[T]
}

// value parameter
class Cons[T](val head: T, val tails: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException

  def tails: Nothing = throw new NoSuchElementException
}


def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

singleton[Int](1)
singleton[Boolean](false)

// Type inference
singleton(1)
singleton(false)

def nth[T](n: Int, xs: List[T]): T =
  if (xs.isEmpty) throw new IndexOutOfBoundsException
  else if (n == 0) xs.head
  else nth(n - 1, xs.tails)

def xs = new Cons(1, new Cons(2, new Cons(3, new Nil)))

nth(2, xs)
nth(-1, xs)







