package horstmann

/**
  * Created by moroz on 11.06.17.
  */
object Section4 extends App {

  object Question1 {
    val wishes = Map("Health" -> 7777777, "Mind" -> 5555555, "Love" -> 3333333)

    val discount = wishes.map((wish) => (wish._1 -> wish._2 * 0.1))
    println(discount.mkString(" , "))
    val same_as_discount = for ((wish, value) <- wishes) yield wish -> value * 0.1
    println(same_as_discount.mkString(" , "))
  }

  object Question2 {

    def scan: Unit = {
      import java.util.Scanner
      import java.io.File
      import scala.collection.mutable.SortedMap
      val scanner = new Scanner(new File("test.data"))
      var words = SortedMap[String, Int]()
      while (scanner.hasNext) {
        val word = scanner.next
        words = words + (word -> (words.getOrElse(word, 0) + 1))
      }
      for (word <- words) println(word._1 + "=" + word._2)
    }
  }

  object Question3 {

    def scan: Unit = {
      import java.util.Scanner
      import java.io.File
      import scala.collection.mutable.SortedMap
      val scanner = new Scanner(new File("test.data"))
      val words = SortedMap[String, Int]()
      while (scanner.hasNext) {
        val word = scanner.next
        words += (word -> (words.getOrElse(word, 0) + 1))
      }
      for (word <- words) println(word._1 + "=" + word._2)
    }
  }

  object Question6 {

    import java.util.Calendar._
    import scala.collection.immutable.ListMap

    val days = ListMap("monday" -> MONDAY, "saturday" -> SATURDAY,
      "thursday" -> THURSDAY, "friday" -> FRIDAY, "tuesday" -> TUESDAY,
      "sunday" -> SUNDAY, "wednesday" -> WEDNESDAY)

    for (day <- days) println(day)
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
  //Question2.scan
  // Question3.scan
  Question6
  println(Question8.minmax(Array(-9000, -9000, 12, 35, -1, -1000, 345678)))
  println(Question9.lteqgt(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 5, 11), 5))
  Question10
}
