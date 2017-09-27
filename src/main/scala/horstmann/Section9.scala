package horstmann

import java.io.{FileOutputStream, PrintWriter, File => JFile}
import java.nio.charset.Charset
import java.nio.{ByteBuffer, CharBuffer}
import java.util.{Scanner => JScanner}
import scala.io.Source

/**
  * Created by moroz on 05.07.17.
  */
object Section9 extends App {

  val resourcesDir = "out/production/resources"

  object l9 {

    object TestCharsets1 {

      println("--------TestCharsets1------")

      //    import scala.collection.JavaConverters._
      //    val c = Charset.availableCharsets().asScala
      //    for (a <- c) {
      //      println(a)
      //    }

      val charset = Charset.forName("koi8-r")

      println("\n---encoded---")
      val bb: ByteBuffer = charset.encode("текст на кириллице")
      val cb = bb.asCharBuffer()
      println(cb.getClass.getSimpleName)
      while (cb.hasRemaining) {
        val c = cb.get()
        print(c) //здесь будут не верные символы поскольку метод bb.asCharBuffer() предполагает наличие в буфере байтов в UTF-16BE
        //see also java.nio.Bits.getCharB + makeChar
      }

      println("\n---decoded---")
      val cb2: CharBuffer = charset.decode(bb)
      while (cb2.hasRemaining) {
        val c = cb2.get()
        print(c)
      }
      println
    }

    object TestBuff1 {
      var charset = Charset.forName("koi8-r")

      val bb1: ByteBuffer = charset.encode("\n")
      println("--------TestBuff1------")
      //val a: Array[Byte] = new Array[Byte](bb1.limit)
      val bb2 = ByteBuffer.allocate(bb1.limit)
      //bb1.flip() //out flipped after encode!
      val bb3 = charset.encode("\n")


      println(bb1.hasRemaining)

      while (bb1.hasRemaining) {
        bb2.put(bb1.get())
      }
      println(bb1.remaining) //zero after read
      println(bb2.remaining) // zero after fill
      bb1.flip()
      bb2.flip()
      println(bb1.remaining)
      println(bb2.remaining)
      println(s"buffers equals? :${bb1.equals(bb2)}")

      charset = Charset.forName("utf-8")

      //bb1.flip() //equals do not touches internal state
      println(bb1.equals(bb3)) //true because '\n' code <127

    }

    object TestBuff2 {

      val buf = ByteBuffer.allocate(10)

      for (x <- 0 until buf.capacity()) {
        buf.put(1.toByte)
        print(s"$x\t")
      }
      println("\nposition:" + buf.position)

    }

    object l91 {
      val a: Byte = 127;
      val b: Int = 127;
      println(a == b) //true
      val c: Byte = 127
      println((c & 0xff)) //127 as Int
      println((c & 0xff).getClass) // int
    }

    object l92 {

      val source = Source.fromFile("section9.q1.data") //impl Iterator[Char]

      //      val buf = source.buffered
      //
      //      while (buf.hasNext) {
      //        print(s" ${buf.head}${buf.next}")
      //      }

      val lines: Iterator[String] = source.getLines //Iterator[String]

      for (line <- lines) println(line)
      println("---")
      source.reset() // do not work ?
      for (char <- source if char != 10) print(char + "\t") //empty -> TraversableOnce
    }

    TestCharsets1
    TestBuff1
    TestBuff2
    l91
    l92
  }

  // l9

  object q2 {

    def replaceTabs(tabStep: Int, fileName: String): Unit = {
      //https://ru.wikipedia.org/wiki/%D0%A2%D0%B0%D0%B1%D1%83%D0%BB%D1%8F%D1%86%D0%B8%D1%8F
      val dst = new PrintWriter(new FileOutputStream(fileName + ".tabs.replaced"))
      val source = Source.fromFile(fileName)

      def writeLine(line: String): Unit = {
        var counter = 0
        line.foreach(char => {
          counter += 1
          if (char == '\t') {
            while (counter < tabStep + 1) {
              dst.print(" ")
              counter += 1
            }
          } else {
            dst.print(char)
          }
          if (counter >= tabStep) counter = 0
        })
        dst.println()
      }

      val lines = source.getLines()

      while (lines.hasNext) {
        writeLine(lines.next())
      }

      dst.flush()
      dst.close()

    }

    replaceTabs(8, s"$resourcesDir/section9.q2.data")
  }

  object q3 {

    import scala.collection.JavaConverters._

    def findWords(minWordLength: Int, fileName: String): Unit = {
      val scanner =
        new JScanner(new JFile(fileName)).asScala

      scanner.filter(_.length >= minWordLength).foreach(println(_))
    }

    findWords(12, s"$resourcesDir/section9.q3.data")

  }

  object q4 {

    import java.util.Locale

    def printStats(fileName: String, desc: String): Unit = {
      val scanner = new JScanner(new JFile(fileName)).useLocale(Locale.US)
      var sum, avg, min, max = 0d
      while (scanner.hasNextDouble) {
        val v = scanner.nextDouble()
        sum += v
        avg += 1 //work as counter in loop
        if ((v > max) || (avg == 1 /*first case*/)) max = v
        if ((v < min) || (avg == 1)) min = v
      }
      avg = sum / avg
      println(s"${desc}: avg=$avg, sum=$sum, min=$min, max=$max")
    }

    printStats(s"$resourcesDir/section9.q4.data", "section9.q4.data")

  }

  //q1 see io/revers junit tests
  q2
  q3
  q4
}
