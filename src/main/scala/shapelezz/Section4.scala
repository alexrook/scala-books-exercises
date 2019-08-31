package shapelezz

object Section4 extends App {

  import shapeless.{HList, ::, HNil}
  import shapeless.ops.hlist.Last
  val last1 = Last[String :: Int :: HNil]
println(last1)
  val last2 = Last[Int :: String :: HNil]



}
