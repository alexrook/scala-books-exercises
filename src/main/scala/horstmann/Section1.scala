package horstmann

object Section1 extends App {

  object math {

    import scala.math._ //package object math

    println(max(3, 2))
    println(sqrt(4))
    //`возможно целое` размером 7 бит
    println(BigInt.probablePrime(7, scala.util.Random))
    println("aaa".count(_ => true)) //3
    //println("aaa".count(_)) //todo ???
  }

  object q4 {
    println("hello" * 3)
  }

  object q6 {

    import scala.math._

    println(pow(2, 1024)) //infinity - double args & ret
    println(BigInt(2) pow 1024)
  }

  object q7 {

    import scala.math.BigInt._
    import scala.util.Random

    for (i <- 0 to 5)
      print(probablePrime(3, Random) + ", ") //max 7 -> 2^3-1, (111) -7 (101) - 5
    println()
  }

  object q8 {

    import scala.util.Random

    println(BigInt(7) toString 2) //111
    println("random dir name:" + (BigInt(Random nextInt) toString 36))
  }

  object q9 {

    println("hello" (0))
    println("hello".last)

  }

  object q10 {
    val s = "scala"
    println(s take 3)
    println(s.substring(0, 3))
    println(s drop 1)
    println(s.substring(1))
    println(s takeRight 3)
    println(s.substring(s.length - 3))
    println(s dropRight 1)
    println(s.substring(0, s.length - 1))

  }

  math
  q4
  println(10 max 2) //q5
  q6
  q7
  q8
  q9
  q10

}
