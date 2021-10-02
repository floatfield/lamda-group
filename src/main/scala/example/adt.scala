package example

object adt {
  val BoolValues: Set[Boolean] = Set(true, false) // car 2

  sealed trait MyBool // car1 + car2

  case object True extends MyBool // ca// car2
  case object False extends MyBool // ca// car2

  sealed trait MyOption[+A] // car[A] + 1 // A + 1

  case object Nothing extends MyOption[Nothing] // car1 == 1
  final case class Some[+A](a: A) extends MyOption[A] // car2

//  type Foo[A, B] = (A, B) // car == car(A) * car(B)
//  final case class Bar[A, B](a: A, b: B)
//
//  val x: Boolean = ???
//  val y: Option[Boolean] = ???
//  val a: Foo[Boolean, Option[Boolean]] = (x, y) // (true, None), (true, Some(true)), (true, Some(false))

  // A = B * (C + D) = B * C + B * D

  // Int
  // Option : Option[Int]
  // Int :: *
  // Option :: * -> *
  // Either :: * -> * -> *
  // Either[Int, _]
  sealed trait Foo[+C, +D]
  final case class Bar[C](c: C) extends Foo[C, Nothing]
  final case class Baz[D](d: D) extends Foo[Nothing, D]

//  final case class Zoo[A, Foo[_, _]](x: A, y: Foo) // Zoo :: * -> (* -> * -> *) -> *
  // val f: A => B
  // f(a) : B
  // val g: A => B => C
  // g(a): B => C
  // g(a)(b): C
  final case class Zoo[A, B, C](x: A, y: Foo[B, C]) // Zoo :: * -> * -> * -> *
  val zoo: Zoo[Int, Boolean, Unit] = Zoo(3, Bar(true))
  
  final case class Quux[A, B](a: A, b: B)
//  def toQuux[A, B](a: (A, B)): Quux[A, B] = Quux(a._1, a._2)
//  def fromQuux[A, B](q: Quux[A, B]): (A, B) = (q.a, q.b)
//  
//  def toQuuxE[A, B](e: Either[A, B]): Quux[A, B] = e match {
//    case Left(l) => ???
//    case Right(r) => ???
//  }
//  def fromQuuxE[A, B](q: Quux[A, B]): Either[A, B] = ???
  
  
  
  type Moose[B, C, D] = Foo[Quux[B, C], Quux[B, D]]
  
  val moose: Moose[Int, Boolean, Unit] = Bar(Quux(3, true))

  //  sealed trait Either[+A, +B]
//  final case class Left[A](c: A) extends Either[A, Nothing, Nothing]
//  final case class Right[A](d: A) extends Either[Nothing, A, Nothing]
//  final case class Top() extends Either[Nothing, Nothing, Nothing]
//  final case class Bottom[D](d: D) extends Either[Nothing, Nothing, D]

//  def f[A, B](x: A): B = ??? // A ^ B
//  val g: Boolean => Option[Boolean] = f(true)


}