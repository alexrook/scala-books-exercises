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
          Cons(head, () => tail().take(n - 1)) //TODO: check laziness
        } else {
          Empty
        }
      case _ => self
    }

    // and  drop(n) for skipping the first n elements of a Stream .
    def drop(n: Int): Stream[A] = self match { //TODO: check laziness
      case Cons(_, tail) if n > 0 => tail().drop(n - 1)
      case _                      => self
    }

    //Write the function takeWhile for returning all starting elements of a Stream that
    //match the given predicate.
    def takeWhile(p: A => Boolean): Stream[A] = self match { //TODO: check laziness
      case Cons(head, tail) =>
        lazy val hd = head()
        if (p(hd)) {
          Cons(() => hd, () => tail().takeWhile(p))
        } else {
          Empty
        }
      case _ => self
    }

    //from book answers
    /*
      It's a common Scala style to write method calls without `.` notation, as in `t() takeWhile f`.
     */
    def takeWhileB(f: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if f(h()) => Stream.cons(h(), t() takeWhile f)
      case _                    => Stream.empty
    }

    def headOption: Option[A] = self match {
      case Cons(head, _) => Some(head())
      case _             => None
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = self match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    //Implement forAll , which checks that all elements in the Stream match a given predicate.
    // Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
    def forAll(p: A => Boolean): Boolean = foldRight(true) { (head, tail) =>
      p(head) && tail
    }

    //Use foldRight to implement takeWhile .
    def takeWhileC(f: A => Boolean): Stream[A] = ???

    //Hard: Implement headOption using foldRight .
    def headOptionA: Option[A] = foldRight(None: Option[A]) { (head, _) =>
      Option(head)
    }

  }

  case object Empty extends Stream[Nothing]

  //  A nonempty stream consists of a head and a tail,  whi ch are both non-strict. Due to technical
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

  assert(str1.takeWhile(_ < 3).toList == List(1, 2))
  assert(str1.takeWhile(_ > 3).toList == List.empty[Int])
  assert(str1.takeWhile(_ => true).toList == str1.toList)
  assert(str1.takeWhile(_ != 42).toList == str1.toList)

  import Stream.cons
  val str2 = cons({ print("a"); 1 }, cons({ print("b"); 2 }, cons({ print("c"); 3 }, Empty)))

  println("check for `drop` laziness")
  println("it should be:")
  println("a==Some(1)==b==Some(2)==c==Some(3)====None==")
  var ret: Option[Int] = _
  var buf: Stream[Int] = str2
  do {
    ret = buf.headOption
    print(s"==$ret==")
    buf = buf.drop(1)
  } while (ret.nonEmpty)

  println("\n---")
  println(str2.takeWhile(_ < 5).toList)
  println(str2.takeWhileB(_ < 5).toList)

  println(str2.forAll(_ < 3))
  println(str2.forAll(_ > 0))

  println(str2.headOptionA)
  println(Stream.empty[String].headOptionA)

}
