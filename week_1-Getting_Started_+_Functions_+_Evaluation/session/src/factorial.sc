import scala.annotation.tailrec

def factorial(n: Int): Int =
  if (n == 0) 1 else n * factorial(n - 1)


factorial(9)

def factorial_tailrec(n: Int): Int = {
  @tailrec
  def loop(acc: Int, n: Int): Int =
    if (n == 0) acc
    else loop(n * acc, n - 1)
  loop(1, n)
}

factorial_tailrec(9)
