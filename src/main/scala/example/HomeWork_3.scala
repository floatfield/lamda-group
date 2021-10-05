package example

import example.MoreRecursion.{Cons, MyIntList, MyList}

import scala.annotation.tailrec

object HomeWork_3 extends App {

  sealed trait List[+A]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]
  case object Empty extends List[Nothing]

  //  val x = List(1,2,3,4,5) match {
//    case Cons(x, Cons(2, Cons(4, _))) => x
//    case Nil => 42
//    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y case Cons(h, t) => h + sum(t)
//    case _ => 101
//  }

  //Answer: 3


  //3.2


  def tailD[A](list: List[A]): List[A] =
    list match {
      case Empty => "empty list")
      case Cons(_,t) => t
    }

  //3.3

  val x = ???

  def setHead[A](list: List[A]): List[A] = {
    list match {
      case Empty => "empty list"
      case Cons(_, tail) => Cons(x, tail)
    }
  }

    //3.4

    def drop[A](list: List[A], n: Int): List[A] = {
            if (n <= 0) list
            else list match {
              case Cons(_, tail) => drop(tail, n-1)
              case Empty => "empty list"
          }
        }



}

