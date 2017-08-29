package horstmann

import scala.collection.mutable.ArrayBuffer


object Section3 extends App {

  def printArray[T](array: Traversable[T]): Unit = if (array.nonEmpty) {
    val x0 = array.head

    if (x0.isInstanceOf[Array[T]]) {
      for (a <- array) printArray(a.asInstanceOf[Array[T]])
      println
    } else {
      println
      for (v <- array) print(v + " ")
    }

  } else {
    println("empty array")
  }

  def printA(a: Iterable[Any]): Unit = {
    def loop(i: Int, iter: Iterable[Any]): Unit = {
      print("(" + i + ")=" + iter.head + " ")
      if (iter.tail != Nil) loop(i + 1, iter.tail)
    }

    println()
    loop(0, a)
  }

  val a = Array[Int](1, 2, 3)
  //printA(a)
  // println(1 > 2)
  val b = ArrayBuffer((for (i <- 0 to 3) yield i): _*)
  b += (1, 2, 3, 4, 5, 6, 7) //+ (A*)
  //  printA(b)

  b ++= Array(23, 67, 89)

  // printA(b)
  // println("\n" + a.mkString("< ", " ", " >"))
  // println(b.max)


  class Triangle(val name: String) {
    override def toString = this.name
  }

  val l = Array(new Triangle("aaa"), new Triangle("bbb"), new Triangle("ccc"))
  val ret = l.reduceLeft((a, b) => b)
  // println(ret)

  val matrix0: Array[Array[Int]] = Array.ofDim[Int](3, 3) //col,row
  // for (x <- matrix0) println(x(0))
  val pojoArray: Array[Int] = Array.ofDim[Int](3)
  matrix0(0)(0) = 1
  pojoArray(0) = 1

  val dynMatrix = new Array[Array[Int]](3) //3 col
  // println("---")
  for (x <- 0 until dynMatrix.length) dynMatrix(x) = new Array[Int](3)


  object q1 {
    def arrayUpToN(n: Int): Array[Int] = {
      val ret = new Array[Int](n)
      for (i <- 0 until n) ret(i) = i
      ret
    }
  }

  object q2 {

    def swapPos[T](a: Array[T]): Unit = {

      def swap(skip: Boolean, index: Int, seq: Iterable[T]): Unit = if (seq.isEmpty) {
        return
      } else if (skip) {
        swap(false, index + 1, seq.tail)
      } else {
        val old = a(index - 1)
        a(index - 1) = a(index)
        a(index) = old
        swap(true, index + 1, seq.tail)
      }

      swap(true, 0, a)
    }
  }

  object q3 {
    def swapPos[T](a: Seq[T]): Seq[String] = ???
  }

  for (x <- 0 until 3; y <- 0 until 3) matrix0(x)(y) = x + y
  printArray(matrix0)
  println("\n---q1")
  printArray(q1.arrayUpToN(5))
  println("\n---q2")
  val q2TestData01 = Array(1, 2, 3, 4, 5)
  printArray(q2TestData01)
  q2.swapPos(q2TestData01)
  printArray(q2TestData01)
  val q2TestData02 = Array(1, 2, 3, 4)
  printArray(q2TestData02)
  q2.swapPos(q2TestData02)
  printArray(q2TestData02)
  println("\n---q3")
  val q2TestData03 = Array(1, 2, 3, 4, 5)
  //printArray(q3.swapPos(q2TestData03))

}
