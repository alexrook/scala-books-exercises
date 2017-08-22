package horstmann

import java.nio.charset.Charset
import java.util.function.LongBinaryOperator

object Section2 extends App {

  object q1 {
    def signum(x: Double): Int = if (x == 0) 0
    else if (x > 0) 1 else -1
  }

  object q2 {
    def a: Unit = {}
  }

  object q3 {
    var y: Int = 1
    var x: Unit = {}
    x = y = 1
  }

  object q4 {
    for (i <- 10.to(0, -1)) print(i + " ")
  }

  object q5 {
    def countdown(n: Int) = for (i <- n to 0 by -1) print(i + {
      if (i > 0) ", " else ""
    })
  }

  object q6 {

    def codePointProduct(s: String): Long = if (s.nonEmpty) {
      println(s.codePointCount(0, s.length))
      println(s.length)
      var ret = 1L;
      for (i <- 0 to s.codePointCount(0, s.length) - 1) {
        println(ret)
        println(s.codePointBefore()
        ret = ret * s.codePointAt(i)
      }
      ret
    } else 0L

    def printChars(s: String) = for (c <- s) print(c + ":" + c.toInt + " ")
  }

  object q7 {
    def codePointProduct(s: String): Long = if (s.nonEmpty) s.codePoints().asLongStream().
      reduce(1, (left: Long, right: Long) => left * right) else 0
  }

  object q8 {
    def codePointProduct(s: String): Long = if (s.nonEmpty) {
      def loop(r: Long, str: String): Long = {
        val ret = r * str.head
        if (str.tail.nonEmpty) loop(ret, str.tail) else ret
      }

      loop(1, s)
    } else 0
  }


  //  print(q1.signum(-12.3))
  //  print(", " + q1.signum(0))
  //  println(", " + q1.signum(45))
  //  println(q2.a.getClass.getName)
  //  q3
  //  q4
  //  println()
  //  q5.countdown(11)
  //  q6.printChars("你好")
  //  println("\n" + q6.codePointProduct("你好"))
  val bytes = Array[Byte](/*Aegean 4 (U+1010A)*/ -16, -112, -124, -118,
    /*Aegean 2 (U+10108) */ -16, -112, -124, -120)
  val aegean42 = new String(bytes, Charset.forName("UTF-8"))
  //q6.printChars(aegean42)
  //println(
    //    "\n" + q6.codePointProduct("Hello") +
    //      ",empty:" + q6.codePointProduct("") +
    //" aegean 42:" + q6.codePointProduct(aegean42))

    println(
  //    //    "\n" + q7.codePointProduct("Hello") +
  //    //      ",empty:" + q7.codePointProduct("") +
      " aegean 42:" + q7.codePointProduct(aegean42))
  //
  //  println(
  //    //    "\n" + q8.codePointProduct("Hello") +
  //    //      ",empty:" + q8.codePointProduct("") +
  //    " aegean 42:" + q8.codePointProduct(aegean42))
  //

}
