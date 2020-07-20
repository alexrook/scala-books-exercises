package shapelezz

import shapeless._
import shapeless.Witness

trait HasEmpty[T] {
  def empty: T
}

object HasEmpty {

  def instance[T](v: T): HasEmpty[T] = new HasEmpty[T] {
    override def empty: T = v
  }

  implicit val booleanHasEmpty: HasEmpty[Boolean] =
    instance(false)

  implicit val stringHasEmpty: HasEmpty[String] =
    instance("")

  implicit val intHAsEmpty: HasEmpty[Int] =
    instance(0)

  implicit def optionHasEmpty[T:HasEmpty]: HasEmpty[Option[T]] = instance(None)

  implicit def listHasEmpty[T: HasEmpty]: HasEmpty[List[T]] = instance(List.empty[T])

  implicit def hListHasEmpty[H, T <: HList]
  (implicit headHasEmpty: Lazy[HasEmpty[H]],
   tailHasEmpty: HasEmpty[T]): HasEmpty[H :: T] = new HasEmpty[H :: T] {
    override def empty: H :: T = headHasEmpty.value.empty :: tailHasEmpty.empty
  }

  implicit val hNilHasEmpty: HasEmpty[HNil] = new HasEmpty[HNil] {
    override def empty: HNil = HNil
  }

  implicit def genHasEmpty[T, Repr](implicit gen: Generic.Aux[T, Repr],
                                    hList: Lazy[HasEmpty[Repr]]): HasEmpty[T] =
    new HasEmpty[T] {
      override def empty: T = {
        gen.from(hList.value.empty)
      }
    }


  def apply[T](implicit ev: HasEmpty[T]): T = ev.empty

}


object heApp extends App {

  case class Holder(a: Boolean, b: String)

  //implicit val genHolder = Generic[Holder]

  val emHolder = HasEmpty[Holder]


  println(emHolder)

}