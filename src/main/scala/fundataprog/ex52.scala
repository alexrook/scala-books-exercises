package fundataprog

object ex52 {

  sealed trait Stream[+A] { self =>

    // Write a function to convert a Stream to a List , which will force its evaluation and let
    // you look at it in the REPL . You can convert to the regular List type in the standard
    // library. You can place this and other functions that operate on a Stream inside the
    // Stream trait.
    def toList: List[A] = self match {
      case Empty            => List.empty[A]
      case Cons(head, tail) => head() +: tail().toList
    }

    //    Write the function take(n) for returning the first n elements of a Stream
    def take(n: Int): Stream[A] = self match {
      case Cons(head, tail) =>
        if (n > 0) {
          Cons(head, () => tail().take(n - 1))
        } else {
          Empty
        }
      case _ => self
    }

    // and  drop(n) for skipping the first n elements of a Stream .
    def drop(n: Int): Stream[A] = self match {
      case Cons(_, tail) =>
        if (n > 0) {
          tail().drop(n - 1)
        } else {
          self
        }
      case _ => self
    }

  }

  case object Empty extends Stream[Nothing]

  //  A nonempty stream consists of a head and a tail,  which are both non-strict. Due to technical
  //  limitations, these are thunks that must be explicitly forced, rather than by-name parameters.
  // case class Holder(a: =>String,b: =>Int) -> error: `val' parameters may not be call-by-name,  case class Holder(a: =>String,b: =>Int)
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  }

}

object test52 extends App {

  import ex52._

  val str1 = Stream(1, 2, 3)

  println(str1.toList)
  println(str1.take(12).toList)
  //take
  assert(str1.take(0).toList == List.empty[Int])
  assert(str1.take(1).toList == List(1))
  assert(str1.take(3).toList == List(1, 2, 3))
  assert(str1.take(-3).toList == List.empty[Int])
  assert(Empty.take(12).toList == List.empty[Nothing])
  //drop
  assert(str1.drop(0).toList == List(1, 2, 3))
  assert(str1.drop(1).toList == List(2, 3))
  assert(str1.drop(3).toList == List.empty[Int])
  assert(str1.drop(-3).toList == List(1, 2, 3))
  assert(Empty.drop(12).toList == List.empty[Nothing])

}
