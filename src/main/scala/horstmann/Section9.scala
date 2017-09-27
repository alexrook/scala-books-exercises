package horstmann

import java.nio.charset.Charset
import java.nio.{ByteBuffer, CharBuffer}
import scala.io.Source

/**
  * Created by moroz on 05.07.17.
  */
object Section9 extends App {

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
      println("--------TestBuff1------")
      var charset = Charset.forName("koi8-r")
      val bb1: ByteBuffer = charset.encode("\n")
      //bb1.flip() //out flipped after encode!

      //val a: Array[Byte] = new Array[Byte](bb1.limit)
      val bb2 = ByteBuffer.allocate(bb1.limit)

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

      val bb3 = charset.encode("\n")
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

    //    TestCharsets1
    //    TestBuff1
    //    TestBuff2
    //    l91
    l92

  }

  l9


}
