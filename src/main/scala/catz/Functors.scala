package catz

import java.awt.print.Printable

import cats.Functor
import cats.instances.string._

import scala.language.higherKinds

object Functors extends App {

  implicit class FunctorOps[F[_], A](src: F[A]) {

    def map[B](func: A => B)(implicit functor: Functor[F]): F[B] =
      functor.map(src)(func)
  }

//  val s:String=""
//
//  s.map((a:String)=>123)(new Functor[Comparable] {
//    override def map[A, B](fa: Comparable[A])(f: A => B): Comparable[B] = new Comparable[B] {
//      override def compareTo(t: B): Int = 0
//    }
//  })

  val i: Int = 1

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value:  A) extends Tree[A]

  val leaf1:  Tree[Int] = Leaf(1)
  val leaf2:  Leaf[Int] = Leaf(2)
  val branch: Tree[Int] = Branch(leaf1, leaf2)

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value)         => Leaf(f(value))
    }
  }

  println(branch.map(i => i * 2))

  trait Printable[A] { self =>
    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] =
      new Printable[B] {
        def format(value: B): String = self.format(func(value))

      }
  }

  implicit val stringPrintable: Printable[String] =
    new Printable[String] {

      def format(value: String): String =
        "\"" + value + "\""
    }
  implicit val booleanPrintable: Printable[Boolean] =
    new Printable[Boolean] {

      def format(value: Boolean): String =
        if (value) "yes" else "no"
    }

  def format[A](v: A)(implicit ev: Printable[A]) = ev.format(v)

  final case class Box[A](value: A)

  implicit def boxIsPrintable[A](implicit ev: Printable[A]): Printable[Box[A]] =
    ev.contramap(_.value)

  println(format("hello"))
  println(format(true))
  println(format(Box("hi")))

  trait Codec[A] { self =>
    def encode(value: A): String
    def decode(value: String): A

    def imap[B](dec:  A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))

      override def decode(value: String): B = dec(self.decode(value))
    }
  }

}
