package catz

import shapeless.Generic.Aux

object App1 extends App {

  import cats.Show
  import cats.instances.string._
  import cats.instances.int._

  import shapeless._

  implicit val hNilIsSHow: Show[HNil] = Show.show(_ => "")

  implicit def hListIsShow[H, T <: HList](implicit head: Show[H], tail: Show[T]): Show[H :: T] =
    (t: H :: T) => {
      val buf = new StringBuilder(head.show(t.head))
      buf.append(":").append(tail.show(t.tail))
      buf.mkString //todo: replace with `t.foldLeft..` or something else

    }

  implicit def genericIsShow[T, R <: HList](implicit gen: Generic.Aux[T, R], repr: Show[R]): Show[T] = (v: T) => {
    val hList: R = gen.to(v)
    repr.show(hList)
  }

  val hl1: String :: Int :: HNil = "aaa" :: 123 :: HNil

  println("console ret == " + Show.apply[String :: Int :: HNil].show(hl1))

  case class Holder(a: String, b: Int)

  implicit val genHolder: Generic[Holder] = Generic[Holder]

  val holder1 = Holder("bbb", 345)

  println("console holder1 == " + Show[Holder].show(holder1))

}
