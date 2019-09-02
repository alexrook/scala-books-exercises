package fundataprog

object ex32_trees {

  sealed trait Tree[+A]
  case class Leaf[A](value:  A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    def fold[A, B](tree: Tree[A], acc: Option[B])(f: A => B)(comb: (B, B) => B): B = tree match {
      case Leaf(value) => acc.map(comb(_, f(value))).getOrElse(f(value))
      case Branch(left, right) =>
        val lRet = fold(left, acc)(f)(comb)
        val rRet = fold(right, acc)(f)(comb)
        comb(lRet, rRet)
    }

    //Write a function size that counts the number of nodes (leaves and branches) in a tree.
    def size(tree: Tree[_]): Int = tree match {
      case _: Leaf[_] => 1
      case Branch(left: Tree[_], right: Tree[_]) => size(left) + size(right)
    }

    // Write a function maximum that returns the maximum element in a Tree[Int] . (Note:
    // In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x and y)
    import scala.math.Ordering
    def max[A](tree: Tree[A])(implicit ev: Ordering[A]) = fold(tree, None)(identity)(ev.max)

    // Write a function depth that returns the maximum path length from the root of a tree
    // to any leaf.
    def depth(tree: Tree[_]): Int = {
      def loop(v: Int, tree: Tree[_]): Int = tree match {
        case _: Leaf[_] => v
        case Branch(left, right) =>
          val lRet = loop(v + 1, left)
          val rRet = loop(v + 1, right)
          lRet max rRet
      }

      loop(0, tree)
    }

    // Write a function map , analogous to the method of the same name on List , that modi-
    // fies each element in a tree with a given function.
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(value)         => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

    // Generalize size , maximum , depth , and map , writing a new function fold that abstracts
    // over their similarities. Reimplement them in terms of this more general function. Can
    // you draw an analogy between this fold function and the left and right folds for List ?
    def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree, None)(v => Leaf(f(v)): Tree[B])(Tree(_, _))
    def size2(tree:      Tree[_]): Int = fold(tree, None)(_ => 1)(_ + _)

    def apply[A](value: A): Tree[A] = Leaf(value)
    def apply[A](left:  Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  }

}

object tex32_trees extends App {

  import ex32_trees._

  import Tree._

  assert(size(Tree(1)) == 1)
  assert(size(Tree(Tree(1), Tree(Tree(2), Tree(3)))) == 3)
  assert(size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3) //the same

  assert(max(Leaf(1)) == 1)
  assert(max(Leaf(0)) == 0)
  assert(max(Leaf(-142)) == -142)
  assert(max(Branch(Leaf(-1), Branch(Leaf(42), Leaf(3)))) == 42)
  assert(max(Branch(Leaf(-1), Branch(Leaf(-42), Leaf(-3)))) == -1)
  assert(depth((Branch(Leaf(-1), Branch(Leaf(-42), Leaf(-3))))) == 2)

  assert(
    depth(Branch(Branch(Leaf(1), Leaf(1)), Branch(Leaf(-42), Leaf(-3))))
      == 2
  )

  assert(
    depth(Branch(Branch(Leaf(1), Branch(Leaf(1), Leaf(1))), Branch(Leaf(-42), Leaf(-3))))
      == 3
  )

  assert(map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_ + 1) == Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))
  assert(map2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_ + 1) == Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))

  assert(size2(Tree(1)) == 1)
  assert(size2(Tree(Tree(1), Tree(Tree(2), Tree(3)))) == 3)
  assert(size2(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 3) //the same

  println("All Ok")

}
