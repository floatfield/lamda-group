package example

import org.jcp.xml.dsig.internal.dom.DOMXMLObject

import scala.language.implicitConversions

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

object retry {
  // constructors
//  case class Trivial() // ()
  // kind :: *
////  final case class Empty[A]() extends MyList[A]
//  def toTrivial(a: Unit): Trivial = Trivial()
//  def fromTrivial(t: Trivial): Unit = ()
//
//  val a: Trivial = Trivial()
//
//  type TrivialAlias = Trivial
//
//  val a1: TrivialAlias = Trivial()
//
//  a == a1 //??
//
//  case object Trivial1
//
//  val t: Trivial1.type = Trivial1
//
//  type Trivial1Alias = Trivial1.type
//
//  case class Unary(value: Int)
  // kind :: *
//
//  val u: Unary = Unary(10)
//  val u2: Unary = Unary(10)
//  u == u2 // true
//  val i: Int = u.value
//
//  case class Binary(a: String, b: Boolean)
//
//  val b: Binary = Binary("Boom", true)
//  val ba: String = b.a
//  val bb: Boolean = b.b

  case class Unary[A](value: A)
  // kind :: * -> *
  type UnaryInt = Unary[Int]
  // kind :: *

  def foo(x: Int): Int = x - 1
  // Int => Int
  foo(10)
  // Int

  val u: Unary[Long] = Unary(10L)
  val u2: Unary[String] = Unary("Boom!")
//  val u3: Unary[Boolean] = Unary[Boolean]("true")

  case class Binary[A, B](a: A, b: B)
  // kinb :: * -> * -> *
  type BinaryOne[B] = Binary[Int, B]
  // kind: * -> *
  type BinaryApplied = BinaryOne[Long] // Binary[Int, Long]
  // kind :: *

  def binary[A, B](x: A): B => Binary[A, B] = y => Binary(x, y)
  // def binary[A, B]: A => B => Binary[A, B] = a => b => Binary(a, b)

  val b: Binary[String, Boolean] = Binary("Boom", true)
  val b1: Binary[String, Boolean] = binary[String, Boolean]("Boom")(true)

  // binary: String => Boolean => Boolean

//  type BinaryAlias[B] = Binary[String, B]
//
//  val b: BinaryAlias[String] = Binary("Boom", 10L)


//  case class Binary1[A](a: A, b: Option[Unit] = None)
//
//  val b3: Binary1[Long] = Binary1(10L)

//  type UnaryAlias[A] = Unary[A]

  // kinds
//  val f = !true
//
//  val f1 = (x: Long) => x > 3L
//
//  val f2 = f1(5)
//
//
//  type Bool = Boolean
//
//  type IntList = List[Int]
//
//  type ListAlias[A] = List[A]


  // constructors and values
//  case class Pug() // case object Pug
//
//  val myPug = Pug()
//
////  case class Husky[A]()
//  // kind :: * -> *
//
////  val myHusky: Husky[Int] = Husky[Int]()
////  val myOtherHusky: Husky[String] = Husky[String]()
//
//  def getHusky(h: Husky[Int]) = ???
////  getHusky(myOtherHusky)
////  myHusky == myOtherHusky // ???
//
//  case class DogueDeBordeaux[Doge](doge: Doge)
//
//  val myDoge: DogueDeBordeaux[Int] = DogueDeBordeaux(10)
//  val badDoge = DogueDeBordeaux[String](10)


//  sealed trait Doggies[A]
//  // kind(Doggies) :: * -> *
//
//  case class Husky[A](value: A) extends Doggies[A]
//  case class Mastiff[A](value: A) extends Doggies[A]
//
//  val doggos: List[Husky[Int]] = List(Husky(10))
//  val doggos1: List[Doggies[Int]] = List(Husky(10))
//
////  val doggos2: List[Doggies[Int]] = Mastiff(12) +: doggos
////  val doggos3: List[Doggies[Int]] = Mastiff(12) +: doggos1
////  val doggos2 = Mastiff("12") +: doggos
////  val doggos3 = Mastiff("12") +: doggos1
//
//  val d: Husky[String] = Husky(10)
//  val d2: Doggies[Int] = Husky(10.0)
//  val d3: Doggies[Int] = Husky(10L)
//
//  val m1: Doggies[String] = Mastiff("Scooby Doo")
//  val m2: Mastiff[String] = Mastiff("Scooby Doo")

  // What is the kind of Doggies?
  // What is the kind of Doggies[String]? // *
  // What is (are) the type of Husky(10)?
  // What is the type of Mastiff("Scooby Doo")?

//  sealed trait Doggies[+A, +B]
//
//  case class Husky[+A](value: A) extends Doggies[A, Nothing]
//  case class Mastiff[+A](value: A) extends Doggies[Nothing, A]
//
//  val d: Husky[Int] = Husky(10)
//  val d1: Doggies[Int, Nothing] = Husky(10)
//
//  val m1: Mastiff[String] = Mastiff("Scooby Doo")
//  val m2: Doggies[Nothing, String] = Mastiff("Scooby Doo")
//
//  val doggos: List[Doggies[Int, String]] = List(d1, m2)

  // small exercise
  case class Price(value: Integer)
  case class Size(value: Integer)

//  def foo(price: Price, size: Size) = ???

  sealed trait Manufacturer
  case object Mini extends Manufacturer
  case object Mazda extends Manufacturer
  case object Tata extends Manufacturer

  sealed trait Airline
  case object PapuAir extends Airline
  case object Catapults extends Airline
  case object TakeYourChancesUnited extends Airline

  sealed trait Vehicle
  case class Car(manufacturer: Manufacturer, price: Price) extends Vehicle
  case class Plane(airline: Airline, size: Size) extends Vehicle

  val myCar: Car = Car(Mini, Price(14000))
  val myCar2: Vehicle = Car(Mini, Price(14000))
  val urCar = Car(Mazda, Price(20000))
  val clownCar = Car(Tata, Price(7000))
  val doge = Plane(PapuAir, Size(10))
  // 1. What is the type of myCar?
  // 2. define functions
  def isCar(vehicle: Vehicle): Boolean = vehicle match {
    case Car(_, _) => true
    case _ => false
  }
  def isPlane(vehicle: Vehicle): Boolean = vehicle match {
    case Plane(_, _) => true
    case _ => false
  }
  def areCars(vehicles: List[Vehicle]): List[Boolean] =
    vehicles.map(isCar)
  // Now we’re going to write a function to tell us the manufacturer of a piece of data:
  def getManu(vehicle: Vehicle): Either[String, Manufacturer] = vehicle match {
    case Car(m, _) => Right(m)
    case Plane(_, _) => Left("planes don't have manufacturers")
  }
  // All right. Let’s say you’ve decided to add the size of the plane as an argument to the Plane constructor.
  // Add that to your datatypes in the appropriate places and change your data and functions appropriately.

  // functions are exponential
//  def foo(a: Option[Boolean]): Boolean = ???
  // card(foo) =  2 ^ 3

//  def foo1(a: Option[Boolean]): Boolean = {
//    a match {
//      case None => false
//      case Some(false) => false
//      case Some(true) => false
//    }
//  }
//
//  def foo2(a: Option[Boolean]): Boolean = {
//    a match {
//      case None => true
//      case Some(false) => false
//      case Some(true) => false
//    }
//  }
//
//  def foo3(a: Option[Boolean]): Boolean = {
//    a match {
//      case None => false
//      case Some(false) => true
//      case Some(true) => false
//    }
//  }
//
//  def foo4(a: Option[Boolean]): Boolean = {
//    a match {
//      case None => false
//      case Some(false) => false
//      case Some(true) => true
//    }
//  }
//
//  def foo5(a: Option[Boolean]): Boolean = {
//    a match {
//      case None => true
//      case Some(false) => true
//      case Some(true) => false
//    }
//  }
//
//  def foo6(a: Option[Boolean]): Boolean = {
//    a match {
//      case None => true
//      case Some(false) => false
//      case Some(true) => true
//    }
//  }
//
//  def foo7(a: Option[Boolean]): Boolean = {
//    a match {
//      case None => false
//      case Some(false) => true
//      case Some(true) => true
//    }
//  }
//
//  def foo8(a: Option[Boolean]): Boolean = {
//    a match {
//      case None => true
//      case Some(false) => true
//      case Some(true) => true
//    }
//  }

}

// if we have time

/**
 * E-COMMERCE - EXERCISE SET 1
 *
 * Consider an e-commerce application that allows users to purchase products.
 */
object credit_card {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a credit card, which must have:
   *
   *  * Number
   *  * Name
   *  * Expiration date
   *  * Security code
   */
  type CreditCard

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product, which could be a physical product, such as a gallon of milk,
   * or a digital product, such as a book or movie, or access to an event, such
   * as a music concert or film showing.
   */
  type Product

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product price, which could be one-time purchase fee, or a recurring
   * fee on some regular interval.
   */
  type PricingScheme
}