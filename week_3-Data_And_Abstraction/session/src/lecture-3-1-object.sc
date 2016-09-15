abstract class IntSet {
  def contains(x: Int): Boolean

  def incl(x: Int): IntSet
}

// This defines a singleton object named Empty
object Empty extends IntSet {
  def contains(x: Int): Boolean = ???

  def incl(x: Int): IntSet = ???
}


object Hello {
  def main(args: Array[String]) = println("hello world!")
}
