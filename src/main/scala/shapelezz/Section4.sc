import shapeless.{HList, ::, HNil}
import shapeless.ops.hlist.Last

val last1 = Last[String :: Int ::Char:: HNil]

last1("a"::12::'a'::HNil)


println(last1)



val last2 = Last[Int :: String :: HNil]




