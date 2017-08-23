package horstmann

import scala.collection.mutable.ArrayBuffer

object Section3 extends App {


  def printA(a: Iterable[Any]): Unit = {
    def loop(i: Int, iter: Iterable[Any]): Unit = {
      print("(" + i + ")=" + iter.head + " ")
      if (iter.tail != Nil) loop(i + 1, iter.tail)
    }

    println()
    loop(0, a)
  }

  val a = Array[Int](1, 2, 3)
  printA(a)
  println(1 > 2)
  val b = ArrayBuffer((for (i <- 0 to 3) yield i): _*)
  b += (1, 2, 3, 4, 5, 6, 7) //+ (A*)
  printA(b)

  b ++= Array(23, 67, 89)
  printA(b)
  println("\n" + a.mkString("< ", " ", " >"))
  println(b.max)

}
