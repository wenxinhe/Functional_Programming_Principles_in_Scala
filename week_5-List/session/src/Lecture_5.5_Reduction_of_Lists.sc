def sum_0(xs: List[Int]): Int = xs match {
  case Nil => 0
  case y :: ys => y + sum(ys)
}

def sum(xs: List[Int]): Int =
  (0 :: xs) reduceLeft ((x, y) => x + y)

def produce(xs: List[Int]): Int =
  (1 :: xs) reduceLeft ((x, y) => x * y)


def concat[T](xs: List[T], ys: List[T]): List[T] =
  (xs foldRight ys) (_ :: _)

// back to reversing list
def reverse[T](xs: List[T]): List[T] =
(xs foldLeft List[T]()) ((xs, x) => x :: xs)

def map[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]()) ((x, us) => f(x) :: us)

map(List(1, 2, 4), (x: Int) => x + 1)

def length[T](xs: List[T]): Int =
  (xs foldRight 0) ((x, n) => n + 1)

length(List(1, 3, 4))