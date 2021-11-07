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

    def setHead(h: A): MyList[A] = {
      self match {
        case Cons(_, tail) => Cons(h, tail)
        case Empty()       => Cons(h, Empty())
      }
    }

    // from book
    def append(a1: MyList[A], a2: MyList[A]): MyList[A] =
      a1 match {
        case Empty()    => a2
        case Cons(h, t) => Cons(h, append(t, a2))
      }

    def push(a: A): MyList[A] =
      self match {
        case Empty()    => Cons(a, Empty())
        case Cons(h, t) => Cons(h, t.push(a))
      }

    def :+(a: A): MyList[A] = push(a)

    def init(): MyList[A] = {
      @tailrec
      def loop(acc: MyList[A], l: MyList[A]): MyList[A] = {
        l match {
          case Empty() => acc
          case Cons(head, tail) =>
            tail match {
              case Cons(_, _) => loop(acc.push(head), tail)
              case _          => acc
            }
        }
      }

      loop(Empty(), self)
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

    def foldLeft[B](z: B)(f: (B, A) => B): B = MyList.foldLeft(self, z)(f)
  }

  object MyList {
    def apply[A](as: A*): MyList[A] = if (as.isEmpty) Empty()
    else Cons(as.head, apply(as.tail: _*))

//    @tailrec
    def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
      case Empty()     => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum2(ns: MyList[Int]): Int = foldRight(ns, 0)((x, y) => x + y)
    def product2(ns: MyList[Double]): Double = foldRight(ns, 1.0)(_ * _)
    def product2(ns: MyList[Int]): Int = foldRight(ns, 1)(_ * _)

//    trait Productable[A] {
//      def product(a: A, b: A): A
//      def zero: A
//    }
//
//    def product21[A: Productable](ns: MyList[A]): A = {
//      val P = implicitly[Productable[A]]
//      foldRight(ns, P.zero)(P.product)
//    }
//    def product22[A](ns: MyList[A])(implicit p: Productable[A]): A = {
////      val P = implicitly[Productable[A]]
//      foldRight(ns, p.zero)(p.product)
//    }
//
//    implicit val doubleProd = new Productable[Double] {
//      override def product(a: Double, b: Double): Double = a * b
//      override def zero: Double = 1.0
//    }
//
//    def doubleProduct(xs: MyList[Double]): Double = product22(xs)

    def length[A](ns: MyList[A]): Int = foldRight(ns, 0)((_, z) => z + 1)

    @tailrec
    def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = as match {
      case Empty()          => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }

//    val xs: MyList[Int] = ???
//
//    foldLeft(xs, List.empty[Int]){ case (acc, x) => x +: acc}
//    foldRight(xs, List.empty[Int]){ case (x, acc) => x +: acc}

    def flip[A, B, C](f: (A, B) => C): (B, A) => C = (b, a) => f(a, b)
    // 3.13 foldLeft in terms of foldRight
    def fl[A, B](as: MyList[A], z: B)(f: (B, A) => B): B =
      foldRight(as.reverse, z)(flip(f))

    // 3.13 foldRight in terms of foldLeft
    def fr[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
      foldLeft(as.reverse, z)(flip(f))

    def sumL(ns: MyList[Int]) = fl(ns, 0)((x, y) => x + y)
    def productL(ns: MyList[Int]) = fl(ns, 1.0)(_ * _)
    def lengthL[A](ns: MyList[A]): Int = fl(ns, 0)((z, _) => z + 1)
    def reverse[A](ns: MyList[A]): MyList[A] =
      fl(ns, Empty(): MyList[A])((acc, el) => Cons(el, acc))

//    @tailrec
    def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = {
      a1 match {
        case Empty()          => a2
        case Cons(head, tail) => Cons(head, append(tail, a2))
      }
    }

//     3.14 append in terms of either foldLeft or foldRight
    def appendFr[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
      fr(a1, a2)((a, z) => Cons(a, z))

    // 3.15
    def flatten[A](as: MyList[MyList[A]]): MyList[A] =
      fr(as, Empty(): MyList[A])(append)

    // EXERCISE 3.16
    // Write a function that transforms a list of integers by adding 1 to each element.
    // (Reminder: this should be a pure function that returns a new List!)
    def plus1(as: MyList[Int]): MyList[Int] = as.map(_ + 1)

    // EXERCISE 3.17
    // Write a function that turns each value in a List[Double] into a String. You can use
    // the expression d.toString to convert some d: Double to a String.
    def toString(as: MyList[Double]): MyList[String] = as.map(_.toString)

    // EXERCISE 3.18
    // Write a function map that generalizes modifying each element in a list while maintaining
    // the structure of the list. Here is its signature:12
    def map[A, B](as: MyList[A])(f: A => B): MyList[B] = as.map(f)

    // EXERCISE 3.19
    // Write a function filter that removes elements from a list unless they satisfy a given
    // predicate. Use it to remove all odd numbers from a MyList[Int].
    def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = as.filter(f)

    // EXERCISE 3.20
    // Write a function flatMap that works like map except that the function given will return
    // a list instead of a single result, and that list should be inserted into the final resulting
    // list. Here is its signature:
    def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] = flatten(
      as.map(f)
    )

    // EXERCISE 3.21
    // Use flatMap to implement filter.
    def filterFm[A](as: MyList[A])(f: A => Boolean): MyList[A] =
      flatMap(as)(a => if (f(a)) Empty() else MyList(a))

    // EXERCISE 3.22
    // Write a function that accepts two lists and constructs a new list by adding corresponding
    // elements. For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
    def addLists(as: MyList[Int], bs: MyList[Int]): MyList[Int] = {
      def loop(
          as: MyList[Int],
          bs: MyList[Int],
          z: MyList[Int]
      ): MyList[Int] = {
        as match {
          case Empty() =>
            bs match {
              case Empty()      => z
              case Cons(bh, bt) => loop(Empty(), bt, bh +: z)
            }
          case Cons(ah, at) =>
            bs match {
              case Empty()      => loop(at, Empty(), ah +: z)
              case Cons(bh, bt) => loop(at, bt, (ah + bh) +: z)
            }
        }
      }

      loop(as, bs, Empty(): MyList[Int]).reverse
    }

    // EXERCISE 3.23
    // Generalize the function you just wrote so that it’s not specific to integers or addition.
    // Name your generalized function zipWith.
    def zipWith[A](as: MyList[A], bs: MyList[A]): MyList[A] = ???

    // EXERCISE 3.24
    // Hard: As an example, implement hasSubsequence for checking whether a List contains
    // another List as a subsequence. For instance, List(1,2,3,4) would have
    // List(1,2), List(2,3), and List(4) as subsequences, among others. You may have
    // some difficulty finding a concise purely functional implementation that is also efficient.
    // That’s okay. Implement the function however comes most naturally. We’ll
    // return to this implementation in chapter 5 and hopefully improve on it. Note: Any
    // two values x and y can be compared for equality in Scala using the expression x == y.
    def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = ???

  }

  final case class Empty[A]() extends MyList[A]
  final case class Cons[A](head: A, tail: MyList[A]) extends MyList[A] {}

  def getBigList(size: Int): MyList[Int] = {
    @tailrec
    def loop(i: Int, acc: MyList[Int]): MyList[Int] = {
      if (i >= size) acc
      else loop(i + 1, scala.util.Random.nextInt(14) +: acc)
    }

    loop(0, Empty()).reverse
  }

  def test(): Unit = {
//    val oneElementList = 3 +: Empty[Int]() // Empty.+:(3) --> Cons(3, Empty())
//    val twoElementList =
//      10 +: oneElementList // Cons(3, Empty()).+:(10) --> Cons(10, Cons(3, Empty()))
//
//    val mappedList = twoElementList.map(_ + 1)
//    val filteredList = mappedList.filter(_ > 10)

    val bigListDamn = getBigList(15)
    println(s"bigListDamn: $bigListDamn")

//    println(s"oneElementList = $oneElementList")
//    println(s"twoElementList = $twoElementList")
//    println(s"mappedList + 1 = $mappedList")
//    println(s"filteredList > 10 = $filteredList")
//    println(s"bigListDamn.length = ${bigListDamn.length}")
//    println(s"bigListDamn.filter(_ > 7) = ${bigListDamn.filter(_ > 7)}")
//    println(s"bigListDamn.take(10) = ${bigListDamn.take(10)}")
//    println(s"bigListDamn.drop(10) = ${bigListDamn.drop(10)}")
//    println(s"bigListDamn.dropWhile(_ < 7) = ${bigListDamn.dropWhile(_ < 7)}")
//    println(s"bigListDamn.takeWhile(_ < 7) = ${bigListDamn.takeWhile(_ < 7)}")
//    println(s"bigListDamn.init() = ${bigListDamn.init()}")
//    println(MyList.sum2(bigListDamn))
//    println(MyList.product2(bigListDamn))
    val y = MyList(1, 2, 3)
    println(s"y: $y")
    val x = MyList.foldRight(MyList(3, 4, 5), Empty(): MyList[Int])(Cons(_, _))
    println(s"x: $x")

    println(s"x($x) + y($y)")
    println(s"append: ${MyList.append(x, y)}")
    println(s"appendFr: ${MyList.appendFr(x, y)}")

    val listOfLists = MyList(bigListDamn, Empty(): MyList[Int], y, x)
    println(s"listOfLists: $listOfLists")

    println(s"flatten: ${MyList.flatten(listOfLists)}")
//    println(MyList.length(x))
//
//    println(MyList.sumL(bigListDamn))
//    println(MyList.productL(bigListDamn))
//    println(MyList.lengthL(x))
//
    println(s"plus1(y): ${MyList.plus1(y)}")
    println(
      s"addLists: ${MyList.addLists(MyList(1, 2, 3), MyList(4, 5, 6))}"
    )
    println(s"")
    println(s"")
    println(s"")

  }
}
