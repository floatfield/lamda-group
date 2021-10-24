package example

import java.util.concurrent.atomic.DoubleAdder
import scala.annotation.tailrec

// implement missing function implementations
object Functions {

  def curry[A, B, C](f: (A, B) => C): A => B => C = (a: A) => (b: B) => f(a, b)
  //  val sum = (a: Int, b: Int) => a + b
  //  val curriedSum = curry(sum)
  //  val cs1 = curriedSum(1) // what type does cs1 have? int -> int
  //  sum(1, 2) == cs1(2) // Will this evaluate to true? yes

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def flip[A, B, C](f: A => B => C): B => A => C = (b: B) => (a: A) => f(a)(b)
  //  val minus = (a: Long) => (b: Long) => a - b
  //  val fSub = flip(minus) (b: Long) => (a: Long) => a - b
  //  val x1 = minus(10L)(1L) 9
  //  val x2 = fSub(10L)(1L) -9

  /* Q: in the red book repo there is no value at the branching point, why ?

   sealed trait Tree[+A]
   case class Leaf[A](value: A) extends Tree[A]
   case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

   object Tree {}
   */

  // implement all methods of BinaryTree without tail recursion
  sealed trait BinaryTree[A] { self =>
    def map[B](f: A => B): BinaryTree[B] = self match {
      case Leaf(label)       => Leaf(f(label))
      case Node(label, l, r) => Node(f(label), l.map(f), r.map(f))
    }
    //  @tailrec
    def foldPreOrder[B](z: B)(f: (B, A) => B): B = self match {
      case Leaf(label) => f(z, label)
      case Node(label, l, r) =>
        val currentNodeResult = f(z, label)
        val lBranchResult = l.foldPreOrder(currentNodeResult)(f)
        r.foldPreOrder(lBranchResult)(f)
    }

    def foldInOrder[B](z: B)(f: (B, A) => B): B = self match {
      case Leaf(label) => f(z, label)
      case Node(label, l, r) =>
        val lBranchResult = l.foldInOrder(z)(f)
        val currentNodeResult = f(lBranchResult, label)
        r.foldInOrder(currentNodeResult)(f)
    }

    def foldPostOrder[B](z: B)(f: (B, A) => B): B = self match {
      case Leaf(label) => f(z, label)
      case Node(label, l, r) =>
        val lBranchResult = l.foldPostOrder(z)(f)
        val rBranchResult = r.foldPostOrder(lBranchResult)(f)
        f(rBranchResult, label)
    }

    override def toString: String = {
      def go(
          acc: Seq[String],
          selfPrefix: String,
          childPrefix: String,
          node: BinaryTree[A]
      ): Seq[String] = {
        node match {
          case Leaf(l) => acc :+ s"$selfPrefix$l"
          case Node(l, left, right) => {
            Seq(s"$selfPrefix$l") ++
              go(acc, s"$childPrefix|-- ", s"$childPrefix|   ", left) ++
              go(acc, s"$childPrefix`-- ", s"$childPrefix    ", right)
          }
          case _ => acc
        }

      }

      go(Seq[String](), "", "", self).mkString("\n")
    }

    def tailrecToString: String = {
      @tailrec
      def go(
          acc: Seq[String],
          stack: Seq[BinaryTree[A]],
          selfPrefix: String,
          childPrefix: String,
          node: BinaryTree[A]
      ): Seq[String] = {
        node match {
          case Leaf(l) => {
            val newAcc = acc :+ s"$selfPrefix$l"
            val newPrefix = childPrefix.dropRight(4)
            val h = stack.head
            h match {
              case End => newAcc // done
              case Node(_, _, _) =>
                go(
                  newAcc,
                  stack.tail,
                  s"$newPrefix`-- ",
                  s"$newPrefix    ",
                  h
                )
              case Leaf(_) =>
                go(
                  newAcc,
                  stack.tail,
                  s"$newPrefix`-- ",
                  s"$newPrefix",
                  h
                )

            }
          }
          case Node(l, left, right) => {
            val newAcc = acc :+ s"$selfPrefix$l"
            val newStack = right +: stack
            go(newAcc, newStack, s"$childPrefix|-- ", s"$childPrefix|   ", left)
          }
          case _ => Seq("boom")
        }

      }

      case object End extends BinaryTree[A] // empty stack flag
      go(Seq[String](), Seq(End), "", "", self).mkString("\n")
    }
  }

  final case class Leaf[A](label: A) extends BinaryTree[A]

  final case class Node[A](label: A, left: BinaryTree[A], right: BinaryTree[A])
      extends BinaryTree[A]

  def test(): Unit = {
    println("Functions object")

    val t1 = Node(
      3,
      Node(1, Node(21, Leaf(41), Leaf(51)), Leaf(222)),
      Node(
        2,
        Node(4, Leaf(41), Leaf(42)),
        Node(21, Node(221, Leaf(221), Leaf(251)), Leaf(2123))
      )
    )
    //     3
    //   /   \
    //  1      2
    // / \    / \
    //21 222 4   5
    // \\     \\
    // 41 51  41 42
    println(t1)
    println("---")
    println(t1.tailrecToString)
    //  3
    //  |-- 1
    //  |   |-- 21
    //  |   |   |-- 41
    //  |   |   `-- 51
    //  |   `-- 222
    //  `-- 2
    //      |-- 4
    //      |   |-- 41
    //      |   `-- 42
    //      `-- 21
    //          |-- 221
    //          |   |-- 221
    //          |   `-- 251
    //          `-- 2123

//    val t2 = t1.map(_ * 2)
//    //     6
//    //   /  \
//    //  1    4
//    //     /  \
//    //    8    10
//    println(s"t2: $t2")
//
//    val f1 = t1.foldPreOrder(List.empty[Int])((acc, x) => acc :+ x)
//    // List(3,1,2,4,5)
//    println(s"f1: $f1")
//
//    val f2 = t1.foldInOrder(List.empty[Int])((acc, x) => acc :+ x)
//    // List(1,3,4,2,5)
//    println(s"f2: $f2")
//
//    val f3 = t1.foldPostOrder(List.empty[Int])((acc, x) => acc :+ x)
//    // List(1,4,5,2,3)
//    println(s"f3: $f3")
  }
}
