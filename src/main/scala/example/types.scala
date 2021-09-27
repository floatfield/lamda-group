package example

// borrowed from https://github.com/jdegoes/functional-scala
object types {
  type ??? = Nothing

  //
  // EXERCISE 1
  //
  // List all values of the type `Boolean`.
  //
  val BoolValues: Set[Boolean] = Set(true, false)

  //
  // EXERCISE 2
  //
  // List all values of the type `Unit`.
  //
  val UnitValues: Set[Unit] = ???

  //
  // EXERCISE 3
  //
  // List all values of the type `Nothing`.
  //
  val NothingValues: Set[Nothing] = Set()

//  def foo[A](x: A): Nothing = ???

  //
  // EXERCISE 4
  //
  // List all values of the type `Either[Unit, Boolean]`.
  //
//  trait Either[A, B]
//  final case class Left[B](a: B) extends Either[Nothing, B]
//  final case class Right[A](a: A) extends Either[A, Nothing]

  val EitherUnitBoolValues: Set[Either[Unit, Boolean]] = Set(Left(()), Right(true), Right(false))

//  def foo(x: Int): Either[Throwable, Int] = {
//    if (x < 3) Left(new Exception("asdasdas"))
//    else Right(x)
//  }
//
//  val res: Either[Throwable, Int] = foo(2)
//  res match {
//    case Left(xx) => ???
//    case Right(x) => ???
//  }
//  val x: Either[Throwable, Int] = res.map(_ + 2)

  //
  // EXERCISE 5
  //
  // List all values of the type `(Boolean, Boolean)`.
  //
  val TupleBoolBoolValues: Set[(Boolean, Boolean)] =
  ???

  //
  // EXERCISE 6
  //
  // List all values of the type `Either[Either[Unit, Unit], Unit]`.
  //
  val EitherEitherUnitUnitUnitValues: Set[Either[Either[Unit, Unit], Unit]] = ???

  //
  // EXERCISE 7
  //
  // Given:
  // A = { true, false }
  // B = { "red", "green", "blue" }
  //
  // List all the elements in `A * B`.
  //
  val AProductB: Set[(Boolean, String)] = ???

  //
  // EXERCISE 8
  //
  // Given:
  // A = { true, false }
  // B = { "red", "green", "blue" }
  //
  // List all the elements in `A + B`.
  //
  val ASumB: Set[Either[Boolean, String]] = ???

  //
  // EXERCISE 9
  //
  // Create a product type of `Int` and `String`, representing the age and
  // name of a person.
  //
  type Person1 = ???
  final case class Person2( /*  */ )

  //
  // EXERCISE 10
  //
  // Prove that `A * 1` is equivalent to `A` by implementing the following two
  // functions.
  //
  def to1[A](t: (A, Unit)): A   = ???
  def from1[A](a: A): (A, Unit) = ???

  //
  // EXERCISE 11
  //
  // Prove that `A * 0` is equivalent to `0` by implementing the following two
  // functions.
  //
  def to2[A](t: (A, Nothing)): Nothing   = ???
  def from2[A](n: Nothing): (A, Nothing) = ???

  //
  // EXERCISE 12
  //
  // Create a sum type of `Int` and `String` representing the identifier of
  // a robot (a number) or the identifier of a person (a name).
  //
  type Identifier1 = ???
  sealed trait Identifier2

  //
  // EXERCISE 13
  //
  // Prove that `A + 0` is equivalent to `A` by implementing the following two
  // functions in a way that loses no information.
  //
  def to3[A](t: Either[A, Nothing]): A   = ???
  def from3[A](a: A): Either[A, Nothing] = ???

  //
  // EXERCISE 14
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // credit card, which has a number, an expiration date, and a security code.
  //
  type CreditCard = ???

  //
  // EXERCISE 15
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // payment method, which could be a credit card, bank account, or
  // cryptocurrency.
  //
  type PaymentMethod = ???

  //
  // EXERCISE 16
  //
  // Create either a sum type or a product type (as appropriate) to represent an
  // employee at a company, which has a title, salary, name, and employment date.
  //
  type Employee = ???

  //
  // EXERCISE 17
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // the rank of a piece on a chess board, which could be a pawn, rook, bishop,
  // knight, queen, or king.
  //
  type ChessPieceRank = ???

  //
  // EXERCISE 18
  //
  // Create a "smart constructor" for `Programmer` that only permits levels
  // that are non-negative.
  //
  sealed abstract case class Programmer private (level: Int)
  object Programmer {
    def fromInt(level: Int): Option[Programmer] = ???
  }

  //
  // EXERCISE 19
  //
  // Using algebraic data types and smart constructors, make it impossible to
  // construct a `BankAccount` with an illegal (undefined) state in the
  // business domain. Note any limitations in your solution.
  //
  final case class BankAccount(ownerId: String, balance: BigDecimal, accountType: String, openedDate: Long)

  //
  // EXERCISE 20
  //
  // Create an ADT model of a game world, including a map, a player, non-player
  // characters, different classes of items, and character stats.
  //
  type GameWorld = ???
}