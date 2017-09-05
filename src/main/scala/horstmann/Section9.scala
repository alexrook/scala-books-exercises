package horstmann
import java.nio.charset.Charset
import java.nio.{ByteBuffer, CharBuffer}
import java.io.File
import java.nio.file.Files
import java.util
import java.nio.file.StandardOpenOption

/**
  * Created by moroz on 05.07.17.
  */
object Section9 extends App {

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
    while (cb.hasRemaining) {
      val c = cb.get()
      print(c) //здесь будут не верные символы поскольку метод bb.asCharBuffer() предполагает наличие в буфере байтов в UTF-16BE
    }

    println("\n---decoded---")
    val cb2 = charset.decode(bb)
    while (cb2.hasRemaining) {
      val c = cb2.get()
      print(c)
    }
    println
  }

  object TestBuff1 {
    println("--------TestBuff1------")
    var charset = Charset.forName("koi8-r")

    val bb1 = charset.encode("\n")

    bb1.flip()

    val a: Array[Byte] = new Array[Byte](bb1.limit)
    val bb2 = ByteBuffer.allocate(bb1.limit)

    while (bb1.hasRemaining) {
      bb2.put(bb1.get())
    }
    println(bb1.equals(bb2))

    charset = Charset.forName("utf-8")

    val bb3 = charset.encode("\n")
    println(bb1.equals(bb3))

  }

  object TestBuff2 {

    val buf = ByteBuffer.allocate(10)

    for (x <- 0 to buf.capacity() - 1) {
      print(x + "\t")
      buf.put(1.toByte)


    }
    println("\nposition:" + buf.position)

  }

  //  TestCharsets1
  //
  //  TestBuff1
  //  TestBuff2

val a:Byte=1;

  val b:Int=1;

  println(a==b)

  val c:Byte= 127
  println((c & 0xff))
  println((c & 0xff).getClass)

}
