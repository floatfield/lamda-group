package example

object Recursion {
  //1. sum up to n
  // sumUpToN(5) == 15 // 1+2+3+4+5 == 15
  def sumUpToN(n: Int): Int = ???

  //2. multiply (via addition)
  def multiply(a: Int, b: Int): Int = ???

  // 1. integral division
  sealed trait DividedByResult

  object DividedByResult {
    final case class Result(quotient: Int, remainder: Int) extends DividedByResult
    case object DividedByZero extends DividedByResult
  }
  // 10 `dividedBy` 5 = (2, 0)
  //
  def dividedBy(a: Int, b: Int): DividedByResult = ???

  //2. numbers into words
  def digitToWord(i: Int): String = ???

  def digits(i: Int): List[Int] = ???

  // 1245 -> "one-two-four-five"
  def wordNumber(i: Int): String = ???




}