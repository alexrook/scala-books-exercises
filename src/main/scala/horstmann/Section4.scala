package horstmann

/**
  * Created by moroz on 11.06.17.
  */
object Section4 extends App {

  def printOption(o: Option[Any]) = o match {
    case Some(v) => println(v)
    case None => println("key not found")
  }

  val lorem =
    """Lorem ipsum dolor sit amet elit adipiscing elit
      |Aenean commodo ligula dolor  Aenean massa  massa massa massa
      |In enim justo elit imperdiet a """.stripMargin


  object learn {

    object l41 {
      val map0 = Map('a' -> 1, 'b' -> 2, 'c' -> 3)

      println(map0('b'))

      import scala.collection.mutable.HashMap

      val mutMap0: HashMap[Char, Int] = HashMap('a' -> 1, 'b' -> 2, 'c' -> 3)
      mutMap0.+('d' -> 7)
      println(mutMap0)

      mutMap0('e') = 8 //update here

      // val ret0 = mutMap0('i') //NoSuchElementException
      val ret0: Option[Int] = mutMap0.get('i')

      printOption(ret0)

      val ret1 = mutMap0.getOrElse('e', -1)
      println(ret1)

      mutMap0 += ('f' -> 1, 'j' -> 2, 'h' -> 3) // map.+=(elems:(Char,Int)*)
      println(mutMap0)
      mutMap0 -= 'a'
      println(mutMap0)

      val str = for ((c, i) <- mutMap0) yield c + ":" + i //сопоставление с образцом Section14
      println(str)
    }

    object l47 {
      val tuple: Tuple3[
        Tuple2[Char, Int],
        Tuple2[Char, Int],
        Tuple2[Char, Int]
        ] = ('a' -> 1, 'b' -> 2, 'c' -> 3) //see ide hints

      println(tuple._2._1) //'b'

      val tuple1 = (1, "a", 's')

      val (first, second, third) = tuple1
      println(first + "," + second + "," + third)

      val (f, _, _) = tuple1
      println(f)
    }

  }

  // learn.l41
  //  learn.l47


  object Question1 {
    val wishes = Map("Health" -> 7777777, "Mind" -> 5555555, "Love" -> 3333333)

    val discount = wishes.map((wish) => (wish._1 -> wish._2 * 0.1))
    println(discount.mkString(" , "))
    val same_as_discount = for ((wish, value) <- wishes) yield wish -> value * 0.1
    println(same_as_discount.mkString(" , "))
  }

  object Question2 {

    import java.io.{File, Writer, FileWriter}

    val tmp: File = File.createTempFile("section4.q2.test.", ".data")
    tmp.deleteOnExit()

    val writer: Writer = new FileWriter(tmp)

    writer.write(lorem)
    writer.close()


    def scan: Unit = {
      import java.util.Scanner

      import scala.collection.mutable.SortedMap
      val scanner = new Scanner(tmp)
      var words = SortedMap[String, Int]()
      while (scanner.hasNext) {
        val word = scanner.next
        words = words + (word -> (words.getOrElse(word, 0) + 1))
      }
      println()
      for (word <- words) print(word._1 + "=" + word._2 + ", ")
    }


  }

  object Question3_4 {

    import java.io.{File, Writer, FileWriter}

    val tmp: File = File.createTempFile("section4.q3_4.test.", ".data")
    tmp.deleteOnExit()

    val writer: Writer = new FileWriter(tmp)

    writer.write(lorem)
    writer.close()

    def scan: Unit = {
      import java.util.Scanner
      val scanner = new Scanner(tmp)
      import scala.collection.immutable.SortedMap
      var words = SortedMap[String, Int]().empty
      while (scanner.hasNext) {
        val word = scanner.next
        words += (word -> (words.getOrElse(word, 0) + 1))
      }
      println()
      for (word <- words) print(word._1 + "=" + word._2 + ", ")
    }
  }

  object Question5 {

    import java.io.{File, Writer, FileWriter}

    val tmp: File = File.createTempFile("section4.q5.test.", ".data")
    tmp.deleteOnExit()

    val writer: Writer = new FileWriter(tmp)

    writer.write(lorem)
    writer.close()

    def scan: Unit = {
      import java.util.Scanner
      import scala.collection.JavaConverters._
      import java.util.TreeMap
      val scanner = new Scanner(tmp)

      var words = new TreeMap[String, Int]().asScala

      while (scanner.hasNext) {
        val word = scanner.next
        words += (word -> (words.getOrElse(word, 0) + 1))
      }
      println()
      for ((w, c) <- words) print(w + "=" + c + ", ")
    }
  }


  object Question6 {

    import java.util.Calendar._
    // List map iterators and traversal methods visit key-value pairs in the order whey were first inserted.
    import scala.collection.immutable.ListMap

    val days = ListMap("monday" -> MONDAY, "saturday" -> SATURDAY,
      "thursday" -> THURSDAY, "friday" -> FRIDAY, "tuesday" -> TUESDAY,
      "sunday" -> SUNDAY, "wednesday" -> WEDNESDAY)

    for (day <- days) println(day)
  }

  object Question7 {

    import scala.collection.JavaConverters._

    val props = System.getProperties.asScala.filter(e => (e._1.startsWith("sun") || (e._2.length > 109)) == false)


    var maxKeyLen = props.keySet.maxBy(s => s.length).length
    var maxValLen = props.values.maxBy(s => s.length).length


    println("".padTo(maxKeyLen + 1, '-') + "-" + "".padTo(maxValLen + 1, '-'))
    for ((k, v) <- props)
      println(k.padTo(maxKeyLen + 1, ' ') + "| " + v.padTo(maxValLen, ' ') + "|")


  }

  object Question8 {

    def minmax(array: Array[Int]): (Int, Int) = {

      def loop(ret: (Int, Int), array: Array[Int]): (Int, Int) = if (array.isEmpty) ret else {
        val min = if (ret._1 > array.head) array.head else ret._1
        val max = if (ret._2 < array.head) array.head else ret._2
        loop((min, max), array.tail)
      }

      loop((Int.MaxValue, -Int.MaxValue), array)
    }
  }

  object Question9 {
    def lteqgt(array: Array[Int], v: Int): (Int, Int, Int) = {
      def loop(ret: (Int, Int, Int), array: Array[Int]): (Int, Int, Int) = {
        if (array.isEmpty) ret else {
          val min = if (v > array.head) ret._1 + 1 else ret._1
          val eq = if (v == array.head) ret._2 + 1 else ret._2
          val gt = if (v < array.head) ret._3 + 1 else ret._3
          loop((min, eq, gt), array.tail)
        }
      }

      loop((0, 0, 0), array)
    }
  }

  object Question10 {
    val zip = "Hello".zip("World")
    println(zip)
  }


  Question1
  Question2.scan
  Question3_4.scan
  Question5.scan
  Question6
  Question7
  println(Question8.minmax(Array(-9000, -9000, 12, 35, -1, -1000, 345678)))
  println(Question9.lteqgt(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 5, 11), 5))
  Question10
}
