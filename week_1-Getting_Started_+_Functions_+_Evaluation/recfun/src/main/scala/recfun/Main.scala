package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
  if (r < 0 || c < 0) 0
  else if (r == 0 && c == 0) 1
  else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def balanceInner(openings: List[Char], chars: List[Char]): Boolean =
      if (chars.isEmpty) openings.isEmpty
      else if (chars.head == '(')
        balanceInner(openings.+:(chars.head), chars.tail)
      else if (chars.head == ')' && openings.nonEmpty)
        balanceInner(openings.dropRight(1), chars.tail)
      else if (chars.head == ')' && openings.isEmpty)
        false
      else balanceInner(openings, chars.tail)

    balanceInner(List(), chars)
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
  if (money == 0) 1
  else if (money < 0 || coins.isEmpty) 0
  else countChange(money, coins.tail) + countChange(money - coins.head, coins)
}
