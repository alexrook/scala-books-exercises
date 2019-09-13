package fundataprog

import scala.annotation.tailrec

object ex43 {

  sealed trait Option[+A] {
//    Implement all of functions on Option . As you implement each function,
//    try to think about what it means and in what situations you’d use it. We’ll explore when
//    to use each of these functions next. Here are a few hints for solving this exercise:
//     It’s fine to use pattern matching, though you should be able to implement all
//      the functions besides map and getOrElse without resorting to pattern matching.
//     For map and flatMap , the type signature should be enough to determine the
//    implementation.
//     getOrElse returns the result inside the Some case of the Option , or if the Option
//      is None , returns the given default value.
//     orElse returns the first Option if it’s defined; otherwise, it returns the second
//      Option .

    def map[B](f: A => B): Option[B] = flatMap(v => Option(f(v)))

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(v) => f(v)
      case _       => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(v) => v
      case _       => default
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case self: Some[A] => self
      case _ => ob
    }

    def filter(f: A => Boolean): Option[A] = flatMap { v =>
      if (f(v)) Some(v) else None
    }

  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {

    trait Div[A] {
      def div(a: A, b: A): A
    }

    object Div {
      implicit def integIsDiv[A](implicit ev: Integral[A]): Div[A] = new Div[A] {
        override def div(a: A, b: A): A = ev.quot(a, b)
      }

      implicit def fractIsDiv[A](implicit ev: Fractional[A]): Div[A] = new Div[A] {
        override def div(a: A, b: A): A = ev.div(a, b)
      }
    }

    //среднее арифметическое
    def mean[A: Numeric](xs: Seq[A])(implicit ev: Div[A]): Option[A] = {
      val num = implicitly[Numeric[A]]
      @tailrec
      def length(acc: A, seq: Seq[_]): A = seq match {
        case _ :: tail => length(num.plus(acc, num.one), tail)
        case _         => acc
      }
      if (xs.isEmpty) None
      else Some(ev.div(xs.sum, length(num.zero, xs)))
    }

    def apply[A](v: A): Option[A] = if (v == null) None else Some(v)

    //  Implement the variance function in terms of flatMap . If the mean of a sequence is m ,
    //  the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
    //  See the definition of variance on Wikipedia (http://mng.bz/0Qsr).
    def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap { m =>
      mean(xs.map { x =>
        math.pow(x - m, 2)
      })
    }

    def lift[A, B](f: A => B): Option[A] => Option[B] = (a: Option[A]) => a.map(f)

    //Write a generic function map2 that combines two Option values using a binary function.
    // If either Option value is None , then the return value is too. Here is its signature:
    def map2[A, B, C](aOpt: Option[A], bOpt: Option[B])(f: (A, B) => C): Option[C] = aOpt.flatMap { a =>
      bOpt.map { b =>
        f(a, b)
      }
    }
    //      for {
    //        a <- aOpt
    //        b <- bOpt
    //      } yield f(a, b)

    // Write a function sequence that combines a list of Option s into one Option containing
    // a list of all the Some values in the original list. If the original list contains None even
    // once, the result of the function should be None ; otherwise the result should be Some
    // with a list of all the values. Here is its signature:
    def sequence[A](list: List[Option[A]]): Option[List[A]] = { //TODO: заменить более функциональным кодом ?
      @tailrec
      def loop(acc: List[A], xs: List[Option[A]]): List[A] = xs match {
        case head :: tail =>
          head match {
            case Some(a) => loop(acc :+ a, tail)
            case None    => List.empty[A]
          }
        case _ => acc
      }

      val l: List[A] = loop(List.empty[A], list)
      if (l.isEmpty) None else Option(l)

    }

    //Implement this function. It’s straightforward to do using map and sequence , but try
    //for a more efficient implementation that only looks at the list once. In fact, imple-
    //  ment sequence in terms of traverse
    def traverse[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] = {

//      val (ret, ok) = list.foldLeft((List.empty[B], true): (List[B], Boolean)) {
//        case ((acc, flag), a) =>
//          if (flag) {
//            f(a) match {
//              case Some(b) => (acc :+ b, true)
//              case _       => (List.empty[B], false)
//            }
//          } else {
//            (List.empty[B], false)
//          }
//      }
//
//      if (ok) Some(ret) else None

      @scala.annotation.tailrec
      def loop(acc: List[B], xs: List[A], flag: Boolean): (List[B], Boolean) = xs match {
        case head :: tail =>
          f(head) match {
            case Some(b) => loop(acc :+ b, tail, true)
            case None    => (acc, false)
          }
        case _ => (acc, flag)
      }

      val (ret, ok) = loop(List.empty[B], list, true)

      if (ok) Some(ret) else None

    }

    def sequence2[A](list: List[Option[A]]): Option[List[A]] = traverse(list)(identity)

  }

} //ex43

object tex43 extends App {

  import ex43._

  assert(Option.mean(Seq(1, 2, 3)) == Some(2))
  assert(Option.mean(Seq(1f, 2.0f, 3f)) == Some(2f))

  println(Option.variance(Seq(1, 2, 53))) //TODO: tests
  println(Option.variance(Seq(1f, 2.0f, 53f)))

  println("a:" + Option.sequence(List(Option(1), Option(2), None)))
  println("b:" + Option.sequence(List(Option(1), Option(2), Some(3))))

  println("c:" + Option.sequence(List.empty[Option[String]]))

  println("d:" + Option.sequence(List(Option(1), None, None)))

  println("tarverse(None expected):" + Option.traverse(List(1, 2, 3))(v => if (v <= 2) Some(v) else None))
  println("tarverse(Some expected):" + Option.traverse(List(1, 2, 3))(Option.apply))
  println("tarverse(Some empty list expected):" + Option.traverse(List.empty[Int])(Option.apply))

  println("sequence (None expected):" + Option.sequence2(List(Some(1), None, Some(3))))
  println("sequence (Some expected):" + Option.sequence2(List(Some(1), Some(2), Some(3))))
  println("sequence (empty list expected):" + Option.sequence2(List.empty[Option[String]]))

}
