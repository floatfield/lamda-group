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
  // implement unimplemented methods of MyList[A] using recursion
  sealed trait MyList[A] { self =>

    def map[B](f: A => B): MyList[B] = {
      @tailrec
      def loop(input: MyList[A], acc: MyList[B]): MyList[B] = {
        input match {
          case Empty()    => acc
          case Cons(h, t) => loop(t, f(h) +: acc)
        }
      }

      loop(self, Empty()).reverse

    }

    def filter(predicate: A => Boolean): MyList[A] = {
      @tailrec
      def loop(input: MyList[A], acc: MyList[A]): MyList[A] = {
        input match {
          case Empty()                    => acc
          case Cons(h, t) if predicate(h) => loop(t, h +: acc)
          case Cons(_, t)                 => loop(t, acc)
        }
      }

      loop(self, Empty()).reverse
    }

    def length: Int = {
      @tailrec
      def loop(input: MyList[A], n: Int): Int = {
        input match {
          case Empty()    => n
          case Cons(_, t) => loop(t, n + 1)
        }
      }

      loop(self, 0)
    }

    def reverse: MyList[A] = {
      @tailrec
      def loop(input: MyList[A], acc: MyList[A]): MyList[A] = {
        input match {
          case Empty()    => acc
          case Cons(h, t) => loop(t, h +: acc)
        }
      }

      loop(self, Empty())
    }

    def take(n: Int): MyList[A] = {
      @tailrec
      def loop(i: Int, input: MyList[A], acc: MyList[A]): MyList[A] = {
        input match {
          case Empty()             => acc
          case Cons(h, t) if i < n => loop(i + 1, t, h +: acc)
          case Cons(_, _)          => acc
        }
      }

      loop(0, self, Empty()).reverse
    }

    def drop(n: Int): MyList[A] = {
      @tailrec
      def loop(i: Int, input: MyList[A]): MyList[A] = {
        input match {
          case Empty()             => Empty()
          case Cons(_, t) if i < n => loop(i + 1, t)
          case Cons(_, t)          => input
        }
      }

      loop(0, self)
    }

    def takeWhile(p: A => Boolean): MyList[A] = {
      @tailrec
      def loop(input: MyList[A], acc: MyList[A]): MyList[A] = {
        input match {
          case Empty()            => acc
          case Cons(h, t) if p(h) => loop(t, h +: acc)
          case Cons(_, _)         => acc
        }
      }

      loop(self, Empty()).reverse
    }

    def dropWhile(p: A => Boolean): MyList[A] = {
      @tailrec
      def loop(input: MyList[A]): MyList[A] = {
        input match {
          case Empty()            => Empty()
          case Cons(h, t) if p(h) => loop(t)
          case Cons(h, t)         => input
        }
      }

      loop(self)
    }

    def +:(a: A): MyList[A] = Cons(a, self)

    override def toString: String = {
      @tailrec
      def loop(input: MyList[A], acc: String): String = {
        input match {
          case Empty()    => acc + "]"
          case Cons(h, t) => loop(t, acc + "," + h)
        }
      }

      self match {
        case Empty()          => "[]"
        case Cons(head, tail) => loop(tail, "[" + head)
      }
    }
  }

  final case class Empty[A]() extends MyList[A]
  final case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

  def test(): Unit = {
    val oneElementList = 3 +: Empty[Int]() // Empty.+:(3) --> Cons(3, Empty())
    val twoElementList =
      10 +: oneElementList // Cons(3, Empty()).+:(10) --> Cons(10, Cons(3, Empty()))

    val mappedList = twoElementList.map(_ + 1)
    val filteredList = mappedList.filter(_ > 10)

    def getBigList(size: Int): MyList[Int] = {
      @tailrec
      def loop(i: Int, acc: MyList[Int]): MyList[Int] = {
        if (i >= size) acc
        else loop(i + 1, scala.util.Random.nextInt(14) +: acc)
      }

      loop(0, Empty()).reverse
    }

    val bigListDamn = getBigList(15)

    println(s"oneElementList = $oneElementList")
    println(s"twoElementList = $twoElementList")
    println(s"mappedList + 1 = $mappedList")
    println(s"filteredList > 10 = $filteredList")
    println(s"bigListDamn = $bigListDamn")
    println(s"bigListDamn.length = ${bigListDamn.length}")
    println(s"bigListDamn.filter(_ > 7) = ${bigListDamn.filter(_ > 7)}")
    println(s"bigListDamn.take(10) = ${bigListDamn.take(10)}")
    println(s"bigListDamn.drop(10) = ${bigListDamn.drop(10)}")
    println(s"bigListDamn.dropWhile(_ < 7) = ${bigListDamn.dropWhile(_ < 7)}")
    println(s"bigListDamn.takeWhile(_ < 7) = ${bigListDamn.takeWhile(_ < 7)}")
  }
}
