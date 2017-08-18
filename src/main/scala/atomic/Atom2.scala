import java.util

import scala.runtime.RichInt

/**
  * Created by moroz on 24.07.17.
  */
object Atom2 extends App {

  def mul(i1: Int)(i2: Int): Int = {
    i1 * i2
  }

  def mul2(i1: Int, i2: Int): Long = {
    i1.toLong * i2
  }

  def square(a: Int) = mul(a)(a)

  def constMul(a: Int): Int => Int = mul(a)

  def testMul() = {

    println(mul(2)(2))
    println(mul(Int.MaxValue)(2))

    println(mul2(Int.MaxValue, 2))

    val f = constMul(3)

    println(f(2))
  }

  // testMul()

  def testList(): Unit = {

    import scala.collection.mutable.ListBuffer
    //  val list = ListBuffer((for (i <- 0 to 100000) yield i):_*)
    var k = -1
    val list = ListBuffer.fill[Int](100000)({
      k += 1;
      k
    })
    // val list = new util.ArrayList[Int]
    //for (i <- 0 to 100000) list += i
    println(list(100))
    val index = 50000
    val start = System.currentTimeMillis()
    for (k <- 0 to 100000) {
      list(index)
    }
    val end = System.currentTimeMillis()
    println(end - start)
  }

  // testList()
  println("aaa" == "aaa")

}
