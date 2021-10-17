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
  val UnitValues: Set[Unit] = Set(())

  //
  // EXERCISE 3
  //
  // List all values of the type `Nothing`.
  //
  val NothingValues: Set[Nothing] = Set()

  //
  // EXERCISE 4
  //
  // List all values of the type `Either[Unit, Boolean]`.
  //
  val EitherUnitBoolValues: Set[Either[Unit, Boolean]] =
    Set(Left(()), Right(true), Right(false))

  //
  // EXERCISE 5
  //
  // List all values of the type `(Boolean, Boolean)`.
  //
  val TupleBoolBoolValues: Set[(Boolean, Boolean)] =
    Set((true, false), (true, true), (false, false), (false, true))

  //
  // EXERCISE 6
  //
  // List all values of the type `Either[Either[Unit, Unit], Unit]`.
  //
  val EitherEitherUnitUnitUnitValues: Set[Either[Either[Unit, Unit], Unit]] =
    Set(Left(Left()), Left(Right(())), Right(()))

  //
  // EXERCISE 7
  //
  // Given:
  // A = { true, false }
  // B = { "red", "green", "blue" }
  //
  // List all the elements in `A * B`.
  //
  val AProductB: Set[(Boolean, String)] = Set(
    (true, "red"),
    (true, "green"),
    (true, "blue"),
    (false, "red"),
    (false, "green"),
    (false, "blue")
  )

  //
  // EXERCISE 8
  //
  // Given:
  // A = { true, false }
  // B = { "red", "green", "blue" }
  //
  // List all the elements in `A + B`.
  //
  val ASumB: Set[Either[Boolean, String]] = Set(
    Left(true),
    Left(false),
    Right("red"),
    Right("green"),
    Right("blue")
  )

  //
  // EXERCISE 9
  //
  // Create a product type of `Int` and `String`, representing the age and
  // name of a person.
  //
  type Person1 = (String, Int)
  final case class Person2(name: String, age: Int)

  //
  // EXERCISE 10
  //
  // Prove that `A * 1` is equivalent to `A` by implementing the following two
  // functions.
  //
  def to1[A](t: (A, Unit)): A = t._1
  def from1[A](a: A): (A, Unit) = (a, ())

  //
  // EXERCISE 11
  //
  // Prove that `A * 0` is equivalent to `0` by implementing the following two
  // functions.
  //
  def to2[A](t: (A, Nothing)): Nothing = t._2
  def from2[A](n: Nothing): (A, Nothing) = throw new NotImplementedError

  //
  // EXERCISE 12
  //
  // Create a sum type of `Int` and `String` representing the identifier of
  // a robot (a number) or the identifier of a person (a name).
  //
  type Identifier1 = Either[Int, String]
  sealed trait Identifier2
  case class Robo(a: Int) extends Identifier2
  case class Hobo(b: String) extends Identifier2
  //
  // EXERCISE 13
  //
  // Prove that `A + 0` is equivalent to `A` by implementing the following two
  // functions in a way that loses no information.
  //
  def to3[A](t: Either[A, Nothing]): A = t match {
    case Left(value) => value
  }
  def from3[A](a: A): Either[A, Nothing] = Left(a)

  //
  // EXERCISE 14
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // credit card, which has a number, an expiration date, and a security code.
  //
  type CreditCard = (Long, String, Int)
  case class CreditCard2(cardNumber: Long, expDate: String, securityCode: Int)

  //
  // EXERCISE 15
  //
  // Create either a sum type or a product type (as appropriate) to represent a
  // payment method, which could be a credit card, bank account, or
  // cryptocurrency.
  //
  //  type PaymentMethod = Either[CreditCard, BankAccount] ???
  sealed trait PaymentMethod
  final case class CreditCard3() extends PaymentMethod
  final case class BankAccount2() extends PaymentMethod
  final case class Cryptocurrency() extends PaymentMethod

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
  final case class BankAccount(
      ownerId: String,
      balance: BigDecimal,
      accountType: String,
      openedDate: Long
  )

  //
  // EXERCISE 20
  //
  // Create an ADT model of a game world, including a map, a player, non-player
  // characters, different classes of items, and character stats.
  //
  type GameWorld = ???
}
