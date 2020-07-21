package shapelezz

object Section2 extends App {

  import shapeless.{::, HNil}

  val product: String :: Int :: Boolean :: HNil =
    "Sunday" :: 1 :: false :: HNil

  println(product)

  val first = product.head
  println(first)
  // first: String = Sunday
  val second = product.tail.head
  println(second)
  // second: Int = 1
  val rest = product.tail.tail
  println(rest)
  // rest: shapeless.::[Boolean,shapeless.HNil] = false :: HNil

  import shapeless.Generic
  case class IceCream(name: String, numCherries: Int, inCone: Boolean)
  val iceCreamGen = Generic[IceCream]
  println(iceCreamGen)

}

object extra extends App {

  import shapeless.{::, Generic, HList, HNil}

  trait Extarpolated[T] {
    def extrapolate(v: T, multiplier: Double): T
  }

  trait Extrapolate[T] {
    def extrapolate(multiplier: Double): T
  }

  object Extrapolate {

    implicit def toExtrapolate[T](v: T)(implicit ev: Extarpolated[T]): Extrapolate[T] = new Extrapolate[T] {
      override def extrapolate(multiplier: Double): T = ev.extrapolate(v, multiplier)
    }

  }

  object Extarpolated {

    def apply[T](implicit ev: Extarpolated[T]): Extarpolated[T] = ev

    def instance[T](func: (T, Double) => T): Extarpolated[T] = new Extarpolated[T] {
      override def extrapolate(v: T, multiplier: Double): T = func(v, multiplier)
    }

    implicit val hNilIsExtrapoltated: Extarpolated[HNil] = instance((_, _) => HNil)

    implicit def hlistExtrapolated[H, T <: HList](implicit extraHead: Extarpolated[H], extraTail: Extarpolated[T]): Extarpolated[H :: T] =
      instance { (hlist, multiplier) =>
        {
          hlist match {
            case head :: tail => extraHead.extrapolate(head, multiplier) :: extraTail.extrapolate(tail, multiplier)
          }
        }
      }

//    implicit def genericIsExtrapolated[T](v: Generic[T])(implicit ev: Extarpolated[v.Repr]): Extarpolated[Generic[T]] = instance {
//      (value, multiplier) =>
//        val a: v.Repr = v.to(value) //представить как hlist, example: a::b::HNil
//        val e: v.Repr = ev.extrapolate(a, multiplier)
//        v.from(e)
//    }

//    implicit def anyIsExtrapolated[T](v: T)(implicit gen: Generic[T], ev: Extarpolated[gen.Repr]): Extarpolated[T] = instance {
//      (value, multiplier) =>
//        val a: gen.Repr =gen.to(value)
//        val e: Aux[T, T] =ev.extrapolate(a,multiplier)
//
//    }

    implicit val longIsExtrapolated: Extarpolated[Long] = instance((v, multiplier) => (v * multiplier).toLong)

  }

  case class Custom(a: String, v: Double)

  implicit val customIsExtrapolated: Extarpolated[Custom] =
    Extarpolated.instance((custom, multiplier) => custom.copy(v = custom.v * multiplier))

  import Extarpolated._
  import Extrapolate._

  val hlist = Custom("a", 4.4) :: 12L :: HNil

  val hlist2 = 32L :: 12L :: HNil

  println(12L.extrapolate(1.2D))

  println(hlist.extrapolate(1.2D))

  println(hlist2.extrapolate(1.2D))

  case class Holder(custom: Custom)

  val holder1 = Holder(Custom("b", 7.7))
  

  val genHolder1 = Generic[Holder]

  val a: ::[Custom, HNil] = genHolder1.to(holder1)

  val b: Holder = genHolder1.from(Custom("d", 1.1) :: HNil)

}

