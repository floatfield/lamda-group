package example

import example.MoreRecursion.{Cons, MyIntList}

object HomeWork_3 extends App {

//  val x = List(1,2,3,4,5) match {
//    case Cons(x, Cons(2, Cons(4, _))) => x
//    case Nil => 42
//    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y case Cons(h, t) => h + sum(t)
//    case _ => 101
//  }

  //Answer: 3


  //3.2
  
  def tail[A](list: List[A]): List[A] =
    list match {
      case Nil => "empty list")
      case Cons(_,t) => t
    }
}
