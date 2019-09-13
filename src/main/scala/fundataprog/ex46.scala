package fundataprog

object ex46 {

  sealed trait Either[+E, +A] { self =>

    //Implement versions of map , flatMap , orElse , and map2 on Either that operate on the
    //Right value.
    def map[B](f: A => B): Either[E, B] = self match {
      case Right(v) => Right(f(v))
      case Left(ex) => Left(ex)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = self match {
      case Right(value) => f(value)
      case Left(ex)     => Left(ex)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = self match {
      case _: Right[A] => self
      case _ => b
    }

    def map2[EE >: E, B, C](other: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        a <- self
        b <- other
      } yield f(a, b)

    //In this implementation, map2 is only able to report one error, even if both the name
    //and the age are invalid. What would you need to change in order to report both errors?
    //Would you change map2 or the signature of mkPerson ? Or could you create a new data
    //type that captures this requirement better than Either does, with some additional
    //structure? How would orElse , traverse , and sequence behave differently for that
    //data type?
    //TODO
  }

  object Either {
    //    Implement sequence and traverse for Either . These should return the first error
    //    that’s encountered, if there is one.
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      @scala.annotation.tailrec
      def loop(acc: List[B], xs: List[A]): Either[E, List[B]] = xs match {
        case head :: tail =>
          f(head) match {
            case Right(v)    => loop(acc :+ v, tail)
            case Left(value) => Left(value)
          }
        case _ => Right(acc)
      }

      loop(List.empty[B], as)

    }
  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

}

object ex46test extends App {

  import ex46._

  val a: Either[String, Int] = Right(12)
  val b: Either[String, Int] = Left("Ex")

  val ret: Either[String, (Int, Int)] = for {
    v1 <- a
    v2 <- b
  } yield (v1, v2)

  println(ret)

  case class Person(name:      Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value:  Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person)

}
