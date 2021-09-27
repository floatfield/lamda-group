package example

import scala.annotation.tailrec

//import example.Recursion.DividedByResult.{DividedByZero, Result}

object Recursion {
  //1. sum up to n
  // sumUpToN(5) == 15 // 1+2+3+4+5 == 15
  def sumUpToN(n: Int): Int = {
    def loop(x: Int, acc: Int): Int = {
      if (x <= 0) acc
      else loop(x - 1, acc + x)
    }

    loop(n, 0)
  }

  //2. multiply (via addition)
  def multiply(a: Int, b: Int): Int = {
    def loop(x: Int, acc: Int): Int = {
      if (x == 1) acc
      else loop(x - 1, acc + a)
    }

    loop(b, 0)

    //    if (b == 1) a
    //    else a + multiply(a, b - 1)
  }

  // 3. integral division
  sealed trait DividedByResult
  // num / denom = quotient * num + remainder
  // 10 / 3 =
  // 10 - 3 = 7 // 1
  // 7 - 3 = 4 //  2
  // 4 - 3 = 1 //  3
  // 1 - 3
  object DividedByResult {
    final case class Result(quotient: Int, remainder: Int)
        extends DividedByResult
    case object DividedByZero extends DividedByResult
  }
  // 10 dividedBy 5 = (2, 0)
  def dividedBy(num: Int, demon: Int): DividedByResult = {
    def loop(x: Int, acc: Int): (Int, Int) = {
      if (x < demon) (acc, x)
      else loop(x - demon, acc + 1)
    }

    if (demon == 0) DividedByResult.DividedByZero
    else DividedByResult.Result.tupled(loop(num, 0))
  }

  val x: DividedByResult = dividedBy(10, 0)

  //  val dig: PartialFunction[Int, String] = {
  //    case
  //  }
  //4. numbers into words
  def digitToWord(i: Int): String = i match {
    case 0 => "zero"
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case 4 => "four"
    case 5 => "five"
    case 6 => "six"
    case 7 => "seven"
    case 8 => "eight"
    case 9 => "nine"
    case _ => "хз unknown digit"
  }

  //1234 / 10 = 123 List(4, 3, 2, 1).reverse
  //1234 % 10 = 4

  // 123 % 10 = 3
  // 123 / 10 = 12

  // 12 % 10 = 2 // n = 12  acc = List(3, 4)
  // 12 / 10 = 1

  // 1
  //

  //  val list = List(1, 2)
  //  val l2 = 3 +: list :+ 4

  // 1234 -> List(1, 2, 3, 4)
  def digits(i: Int): List[Int] = {
    def loop(n: Int, acc: List[Int]): List[Int] = {
      if (n < 10) n +: acc
      else loop(n / 10, n % 10 +: acc)
    }

    loop(i, Nil)
  }

  // 1245 -> "one-two-four-five"
  // homework: fix this. if we run the program in sbt it will output "-one-two-three-four-five-zero" for 123450
  def wordNumber(i: Int): String = {

    def loop(list: List[Int], acc: String): String = {
      list.headOption match {
        case None => acc
        case Some(x) =>
          val newAcc = s"$acc-${digitToWord(x)}"
          loop(list.tail, newAcc)
      }
    }
    loop(digits(i), "")
    digits(i).map(digitToWord).mkString("", "-", "")
  }

}

object MoreRecursion {

  // implement unimplemented methods of MyIntList using recursion
  trait MyIntList { self =>
    def map(f: Int => Int): MyIntList = {
      def loop(l: MyIntList, acc: MyIntList): MyIntList = l match {
        case Empty => Empty
        case Cons(head, tail) =>
          loop(tail, f(head) +: acc) // а тут нельзя так head.f ?
      }

      loop(self, Empty).reverse
    }
    def filter(f: Int => Boolean): MyIntList = {
      def loop(l: MyIntList, acc: MyIntList): MyIntList = {
        l match {
          case Empty => acc
          case Cons(head, tail) => {
            if (f(head)) loop(tail, head +: acc)
            else loop(tail, acc)
          }
        }
      }
      loop(self, Empty).reverse
    }

    def length: Int = {
      def loop(l: MyIntList, acc: Int): Int = l match {
        case Empty         => acc
        case Cons(_, tail) => loop(tail, acc + 1)
      }

      loop(self, 0)
    }

    def reverse: MyIntList = {
      @tailrec
      def loop(l: MyIntList, acc: MyIntList): MyIntList = l match {
        case Empty            => acc
        case Cons(head, tail) => loop(tail, head +: acc)
      }
      loop(self, Empty)
    }

    def take(n: Int): MyIntList = { //взять первые n элементов
      @tailrec
      def loop(l: MyIntList, i: Int, acc: MyIntList): MyIntList = l match {
        case Empty => acc
        case Cons(head, tail) => {
          if (i < n) loop(tail, i + 1, head +: acc)
          else acc
        }
      }
      loop(self, n, Empty).reverse
    }

    def drop(n: Int): MyIntList = { //удалить первые n элементов
      @tailrec
      def loop(l: MyIntList, i: Int): MyIntList = l match {
        case Empty => Empty // чот не понимаю ок это или нет
        case Cons(head, tail) => {
          if (i == n) tail
          else loop(tail, i + 1)
        }
      }
      loop(self, n)
    }

    def takeWhile(p: Int => Boolean): MyIntList = ???
    // тоже самое что фильтр?
//    def filter(f: Int => Boolean): MyIntList = {
//      def loop(l: MyIntList, acc: MyIntList): MyIntList = {
//        l match {
//          case Empty => acc
//          case Cons(head, tail) => {
//            if (f(head)) loop(tail, head +: acc)
//            else loop(tail, acc)
//          }
//        }
//      }
//      loop(self, Empty).reverse
//    }
    def dropWhile(p: Int => Boolean): MyIntList = ???
    def +:(a: Int): MyIntList = Cons(a, self)
  }

  case object Empty extends MyIntList
  final case class Cons(head: Int, tail: MyIntList) extends MyIntList

  val oneElementList = 3 +: Empty
  val twoElementList = 10 +: oneElementList

  val mappedList = twoElementList.map(_ + 1)
  val filteredList = mappedList.filter(_ > 10)

}
