package horstmann

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object Section3 extends App {

  def printArray[T](array: Traversable[T]): Unit = if (array.nonEmpty) {
    val x0 = array.head
    if (x0.getClass.isArray) {
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

  object thinking {
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
    for (x <- 0 until 3; y <- 0 until 3) matrix0(x)(y) = x + y
    printArray(matrix0)

    val tr = ArrayBuffer(1, 2, 3, 45)
    tr.trimEnd(2)
    printArray(tr)
  }


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
    def swapPos[T](a: Seq[T]): Seq[T] =
      for {
        i <- 1 to a.length by 2
        k <- (i) to (i - 1) by -1 if k < a.length
      } yield a(k)
  }


  object q4 {

    def getPosAndNeg(a: Seq[Int]): Seq[Int] = {
      val ret = new Array[Int](a.length)
      val neg = new Array[Int](a.length)

      var retPos = 0
      var negPos = 0

      for (v <- a) {
        if (v >= 0) {
          ret(retPos) = v
          retPos += 1
        } else {
          neg(negPos) = v
          negPos += 1
        }
      }

      for (i <- 0 until negPos) {
        ret(retPos) = neg(i)
        retPos += 1
      }

      ret

    }

  }

  object q5 {
    //   def avg(a: Array[Double]): Double = a.sum / a.count(_ => true)

    def avg[T](a: Array[T])(implicit ev: T => Double): Double = if (a.nonEmpty) {
      var count: Int = 0
      var sum: Double = 0
      for (v <- a) {
        count += 1
        sum += v
      }

      sum / count
    } else {
      0
    }

  }


  object q6 {
    def sortAndRevers[T](a: Seq[T])(implicit ev: Ordering[T]) = a.sorted.reverse
  }

  object q7 {
    def printUnique[Any](a: Seq[Any]): Unit = {
      val ad = a.distinct
      for (v <- ad) print(v + " ")
    }
  }

  object q8 {

    import scala.collection.mutable.Buffer

    //Scala for Impatient $3.4
    def delNegFromHorstmann[T](a: Buffer[T])(implicit ev: Numeric[T]): Buffer[T] = {
      var first = true
      val zero = ev.zero

      val posIndexes = for (i <- 0 until a.length if (first) || (ev.gteq(a(i), zero))) yield {
        if (ev.lt(a(i), zero)) first = false
        i
      }

      for (k <- 0 until posIndexes.length) a(k) = a(posIndexes(k))
      a.trimEnd(a.length - posIndexes.length)

      a

    }

    def delNegIm[T](a: Buffer[T])(implicit ev: Numeric[T]): Buffer[T] = { //immutable
      var first = true
      val zero = ev.zero
      val ret = for (v <- a if (first) || (ev.gteq(v, zero))) yield {
        if (ev.lt(v, zero)) first = false
        v
      }
      ret
    }

    def delNegM[T](a: Buffer[T])(implicit ev: Numeric[T]): Buffer[T] = { //mutable
      var first = true
      val zero = ev.zero
      val neg = for (i <- 0 until a.length if (first) || (ev.lt(a(i), zero))) yield {
        if (ev.lt(a(i), zero)) first = false
        i
      }

      for (k <- (0 until neg.length).reverse) a.remove(k)
      a
    }
  }

  object q9 {
    def timeZoneIDs(prefix: String): Seq[String] = {
      import java.util.TimeZone._
      val ids = getAvailableIDs
      val ret = for (id <- ids if id.startsWith(prefix)) yield id.drop(prefix.length)
      ret.sorted
    }
  }

  object q10 {
    def systemFlavors(): mutable.Buffer[String] = {
      import java.awt.datatransfer._
      import scala.collection.JavaConverters._
      val flavors: SystemFlavorMap = SystemFlavorMap.getDefaultFlavorMap.asInstanceOf[SystemFlavorMap]
      val ret = flavors.getNativesForFlavor(DataFlavor.imageFlavor)
      ret.asScala
    }
  }

  //  thinking

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
  val q3TestData01 = Array(1, 2, 3, 4, 5)
  printArray(q3.swapPos(q3TestData01))
  val q3TestData02 = Array(1, 2, 3, 4)
  printArray(q3.swapPos(q3TestData02))
  println("\n---q4")
  val q4TestData01 = Array(-1, 2, -3, 4, 5)
  printArray(q4.getPosAndNeg(q4TestData01)) //2, 4, 5, -1, -3
  val q4TestData02 = Array(0, 1, 2, 3, 4)
  printArray(q4.getPosAndNeg(q4TestData02)) //0,1,2,3,4

  println("\n---q5")
  val q5TestData01 = Array(-1d, 2, -3.3, 4, 5)
  println(q5.avg(q5TestData01))
  val q5TestData02 = Array(-1, 2, -3, 4, 5)
  println(q5.avg(q5TestData02))
  val q5TestData03 = Array(-1L, 2, -3, 4, 5)
  println(q5.avg(q5TestData03))
  val q5TestData04 = Array("aaa", "bbb", "ccc")
  // println(q5.avg(q5TestData04))
  println("\n---q6")
  val q6td01 = Array(-1, 2, -3, 4, 5)
  printArray(q6.sortAndRevers(q6td01))
  val q6td02 = Array(1d, 3, 7, 4, 5)
  printArray(q6.sortAndRevers(q6td02)) //1,3,4,5,7 -> 7,5,4,3,1
  val q6td03 = ArrayBuffer((for (i <- 0 to 7) yield i): _*)
  printArray(q6.sortAndRevers(q6td03))

  println("\n---q7")
  val q7td01 = Array(-1, -1, 1, 2, -3, 4, 5, 5)
  q7.printUnique(q7td01)
  println()
  val q7td02 = Array(-1, 1, 2, -3.1, -3.1, 1, 4, 5, 5)
  q7.printUnique(q7td02)


  println("\n---q8")
  //println(0d.asInstanceOf[Int])

  val q8td01 = ArrayBuffer(-1, -1, 1, 2, -3, 4, 5, 5)
  var start = System.nanoTime()
  printArray(q8.delNegFromHorstmann(q8td01)) //-1, 1, 2, 4, 5, 5
  println("delNegFromHorstmann time:" + (System.nanoTime() - start))


  val q8td02 = ArrayBuffer(-1, 1, 2, -3.1, -3.1, 1, 4, 5, 5)
  start = System.nanoTime()
  printArray(q8.delNegFromHorstmann(q8td02)) //-1, 2, 1, 4, 5, 5
  println(" time:" + (System.nanoTime() - start))

  val q8td03 = ArrayBuffer(-1, -1, 1, 2, -3, 4, 5, 5)
  start = System.nanoTime()
  printArray(q8.delNegIm(q8td03)) //-1, 1, 2, 4, 5, 5
  println("delNegIm time:" + (System.nanoTime() - start))

  val q8td04 = ArrayBuffer(-1, 1, 2, -3.1, -3.1, 1, 4, 5, 5)
  start = System.nanoTime()
  printArray(q8.delNegM(q8td04)) //-1, 2, 1, 4, 5, 5
  println("delNegM time:" + (System.nanoTime() - start))

  println("\n---q9")
  printArray(q9.timeZoneIDs("America/"))

  println("\n---q10")
  printArray(q10.systemFlavors())

}
