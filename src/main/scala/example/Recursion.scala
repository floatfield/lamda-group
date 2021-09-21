package example

import scala.annotation.tailrec

//import example.Recursion.DividedByResult.{DividedByZero, Result}

object Recursion {
  //1. sum up to n
  // sumUpToN(5) == 15 // 1+2+3+4+5 == 15
  def sumUpToN(n: Int): Int = {
    @tailrec
    def loop(x: Int, acc: Int): Int = {
      if (x <= 0) acc
      else loop(x - 1, acc + x)
    }

    loop(n, 0)
  }

  //2. multiply (via addition)
  def multiply(a: Int, b: Int): Int = {
    @tailrec
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
  // 10 `dividedBy` 5 = (2, 0)
  def dividedBy(num: Int, demon: Int): DividedByResult = {
    @tailrec
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
    @tailrec
    def loop(n: Int, acc: List[Int]): List[Int] = {
      if (n < 10) n +: acc
      else loop(n / 10, n % 10 +: acc)
    }

    loop(i, Nil)
  }

  // 1245 -> "one-two-four-five"
  // homework: fix this. if we run the program in sbt it will output "-one-two-three-four-five-zero" for 123450
  def wordNumber(i: Int): String = {
    // digits(i).map(digitToWord).mkString("-")
    @tailrec
    def loop(list: List[Int], acc: String): String = {
      list.headOption match {
        case None => acc
        case Some(x) =>
          val newAcc = s"$acc-${digitToWord(x)}"
          loop(list.tail, newAcc)
      }
    }

    val head :: tail = digits(i)
    loop(tail, digitToWord(head))
  }

}

object MoreRecursion {
  // implement unimplemented methods of MyIntList using recursion
  trait MyIntList { self =>
    def map(f: Int => Int): MyIntList = ???
    def filter(f: Int => Boolean): MyIntList = ???
    def length: Int = ???
    def reverse: MyIntList = ???

    /** Selects the first n elements */
    def take(n: Int): MyIntList = ???

    /** Selects all elements except first n ones */
    def drop(n: Int): MyIntList = ???
    def takeWhile(p: Int => Boolean): MyIntList = ???
    def dropWhile(p: Int => Boolean): MyIntList = ???

    /** prepend */
    def +:(a: Int): MyIntList = Cons(a, self)
  }

  case object Empty extends MyIntList { self =>
    override def map(f: Int => Int): MyIntList = Empty
    override def filter(f: Int => Boolean): MyIntList = Empty
    override def length: Int = 0
    override def reverse: MyIntList = Empty
    override def take(n: Int): MyIntList = Empty
    override def drop(n: Int): MyIntList = Empty
    override def takeWhile(p: Int => Boolean): MyIntList = Empty
    override def dropWhile(p: Int => Boolean): MyIntList = Empty
  }

  final case class Cons(head: Int, tail: MyIntList) extends MyIntList { self =>
    override def map(f: Int => Int): MyIntList = {
      @tailrec
      def loop(in: MyIntList, acc: MyIntList): MyIntList = {
        in match {
          case Empty      => acc
          case Cons(h, t) => loop(t, f(h) +: acc)
        }
      }

      loop(self, Empty).reverse
    }

    override def filter(f: Int => Boolean): MyIntList = {
      @tailrec
      def loop(in: MyIntList, acc: MyIntList): MyIntList = {
        in match {
          case Empty => acc
          case Cons(h, t) => {
            if (f(h)) loop(t, h +: acc)
            else loop(t, acc)
          }
        }
      }

      loop(self, Empty).reverse
    }

    override def length: Int = {
      @tailrec
      def loop(n: Int, acc: MyIntList): Int = {
        acc match {
          case Empty      => n
          case Cons(h, t) => loop(n + 1, t)
        }
      }

      loop(0, self)
    }

    override def reverse: MyIntList = {
      @tailrec
      def loop(in: MyIntList, acc: MyIntList): MyIntList = {
        in match {
          case Empty      => acc
          case Cons(h, t) => loop(t, h +: acc)
        }
      }

      loop(self, Empty)
    }

    override def take(n: Int): MyIntList = {
      @tailrec
      def loop(i: Int, in: MyIntList, acc: MyIntList): MyIntList = {
        in match {
          case Empty => acc
          case Cons(h, t) => {
            if (i < n) loop(i + 1, t, h +: acc)
            else acc
          }
        }
      }

      loop(0, self, Empty).reverse
    }

    override def drop(n: Int): MyIntList = {
      @tailrec
      def loop(i: Int, in: MyIntList, acc: MyIntList): MyIntList = {
        in match {
          case Empty => acc
          case Cons(h, t) => {
            if (i < n) loop(i + 1, t, acc)
            else loop(i + 1, t, h +: acc)
          }
        }
      }

      loop(0, self, Empty).reverse
    }

    override def takeWhile(p: Int => Boolean): MyIntList = {
      @tailrec
      def loop(in: MyIntList, acc: MyIntList): MyIntList = {
        in match {
          case Empty => acc
          case Cons(h, t) => {
            if (p(h)) loop(t, h +: acc)
            else loop(t, acc)
          }
        }
      }

      loop(self, Empty).reverse
    }

    override def dropWhile(p: Int => Boolean): MyIntList = {
      @tailrec
      def loop(in: MyIntList, acc: MyIntList): MyIntList = {
        in match {
          case Empty => acc
          case Cons(h, t) => {
            if (p(h)) loop(t, acc)
            else h +: t
          }
        }
      }

      loop(self, Empty)
    }

    override def toString: String = {
      @tailrec
      def loop(in: MyIntList, acc: String): String = {
        in match {
          case Empty => acc
          case Cons(h, t) => {
            t match {
              case Empty      => loop(t, acc + h + "]")
              case Cons(_, _) => loop(t, acc + h + ",")
            }
          }
        }
      }

      loop(self, "[")
    }
  }

  val oneElementList = 3 +: Empty
  val twoElementList = 10 +: oneElementList

  val mappedList = twoElementList.map(_ + 1)
  val filteredList = mappedList.filter(_ > 10)

  var longerList = 100 +: oneElementList
  longerList = 101 +: longerList
  longerList = 102 +: longerList
  longerList = 103 +: longerList
  longerList = 104 +: longerList
  longerList = 105 +: longerList

  def getBigList(size: Int): MyIntList = {
    @tailrec
    def loop(i: Int, acc: MyIntList): MyIntList = {
      if (i >= size) acc
      else loop(i + 1, i +: acc)
    }

    loop(0, Empty).reverse
  }

  val bigListDamn = getBigList(100)

  def print(): Unit = {
    println(s"oneElementList = ${oneElementList}")
    println(s"twoElementList = ${twoElementList}")
    println(s"mappedList + 1 = ${mappedList}")
    println(s"filteredList > 10 = ${filteredList}")
    println(s"filteredLongerList > 10 = ${longerList.filter(_ > 1)}")
    println(s"bigListDamn > 10 = ${bigListDamn}")
    println(s"bigListDamn.take(10) = ${bigListDamn.take(10)}")
    println(s"bigListDamn.drop(10) = ${bigListDamn.drop(10)}")
    println(s"bigListDamn.dropWhile(_ < 7) = ${bigListDamn.dropWhile(_ < 7)}")
    println(s"bigListDamn.takeWhile(_ < 7) = ${bigListDamn.takeWhile(_ < 7)}")

  }

}
