package example

// implement missing function implementations
object Functions {

  def curry[A, B, C](f: (A, B) => C): A => B => C = ???
//  val sum = (a: Int, b: Int) => a + b
//  val curriedSum = curry(sum)
//  val cs1 = curriedSum(1) // what type does cs1 have?
//  sum(1, 2) == cs1(2)  // Will this evaluate to true?

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = ???

  def flip[A, B, C](f: A => B => C): B => A => C = ???
//  val minus = (a: Long) => (b: Long) => a - b
//  val fSub = flip(minus)
//  val x1 = minus(10L)(1L)
//  val x2 = fSub(10L)(1L)

  // implement all methods of BinaryTree without tail recursion
  sealed trait BinaryTree[A] {
    def map[B](f: A => B): BinaryTree[B] = ???
    def foldPreOrder[B](z: B)(f: (B, A) => B): B = ???
    def foldInOrder[B](z: B)(f: (B, A) => B): B = ???
    def foldPostOrder[B](z: B)(f: (B, A) => B): B = ???
    override def toString: String = ???
  }

  final case class Leaf[A](label: A) extends BinaryTree[A]
  final case class Node[A](label: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

  val t1 = Node(3, Leaf(1), Node(2, Leaf(4), Leaf(5)))
  //     3
  //   /  \
  //  1    2
  //     /  \
  //    4    5

  val t2 = t1.map(_ * 2)
  //     6
  //   /  \
  //  1    4
  //     /  \
  //    8    10

  val f1 = t1.foldPreOrder(List.empty[Int])((acc, x) => acc :+ x)
  // List(3,1,2,4,5)

  val f2 = t1.foldInOrder(List.empty[Int])((acc, x) => acc :+ x)
  // List(1,3,4,2,5)

  val f3 = t1.foldPostOrder(List.empty[Int])((acc, x) => acc :+ x)
  // List(1,4,5,2,3)
}