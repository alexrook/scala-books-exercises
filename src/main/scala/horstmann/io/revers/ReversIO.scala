package horstmann.io.revers

import java.io.{File, InputStream}
import java.nio.ByteBuffer
import java.nio.file.{Files, StandardOpenOption}
import java.util

/**
  * читает файл построчно - в обратном порядке
  * i.e
  * line1
  * line2
  * line3
  *
  * будет прочитанно как
  * line3
  * line2
  * ....
  *
  * @param lineSeparator - line separator as array of bytes ex: "\n".getBytes() or "<-->".getBytes
  * @param revFileIS     - the stream that reads the file in reverse order
  */
class FileReversLineReader(lineSeparator: Array[Byte],
                           revFileIS: FileReversInputStream) {
  type Line = List[Int]

  def readLine(): Option[Line] = {
    import FileReversLineReader._

    var line = List[Int]()
    val buf = new Array[Int](lineSeparator.length)
    var pos = buf.length - 1

    def loopStreamFunc(i: Int): Boolean = if (pos >= 0) {
      buf(pos) = i
      if (matchTails(lineSeparator, buf, pos)) {
        if (pos == 0) { //сепаратор и буфер полностью совпали
          false //выход из loopStream
        } else { //частичное совпадение,продолжаем наполнять буфер
          pos = pos - 1
          true
        }
      } else {
        line = append(line, buf, pos)
        pos = buf.length - 1
        true
      }
    } else {
      line = append(line, buf, pos)
      pos = buf.length - 1
      true
    }


    if (loopStream(revFileIS, i => {
      loopStreamFunc(i)
    })) {
      Option(line)
    } else {
      line match {
        case _ :: _ => Option(line)
        case _ => None
      }
    }

  }

}

object FileReversLineReader {

  def append[T](line: List[T], head: Array[T], pos: Int): List[T] = {
    var ret = line
    for (i <- head.length - 1 to pos) {
      ret = head(i) :: ret
    }
    ret
  }

  def matchTails[T, E](model: Array[T], src: Array[E], srcPos: Int): Boolean = {

    def loop(i: Int, shift: Int): Boolean = {
      if (src(i) == model(shift + i)) {
        if (i == srcPos) true
        else loop(i - 1, shift)
      } else false
    }

    (srcPos >= 0) && (src.length <= model.length) &&
      loop(src.length - 1, model.length - src.length)

  }


  def loopStream(is: InputStream, f: Int => Boolean): Boolean = {
    val b: Int = is.read()
    if (b != -1) {

      if (f(b)) {
        loopStream(is, f)
      } else true //поток еще не прочинан полностью

    } else false //поток прочитан полностью
  }
}

/**
  * Reads the file in reverse order byte by byte
  *
  * @param file
  * @param readBuffSize read buffer size
  */
class FileReversInputStream(val file: File,
                            val readBuffSize: Int = 1024) extends InputStream {


  private val readBuffer = ByteBuffer.allocateDirect(readBuffSize)
  private var unsignedBytes: Revers[Int] = new ReversUnsignedIntBuffer(readBuffer, -1)

  val channel = Files.newByteChannel(file.toPath,
    util.EnumSet.of(StandardOpenOption.READ))

  channel.position(channel.size())

  var last = channel.position


  override def close(): Unit = {
    if (channel != null) channel.close()
    readBuffer.clear()
    super.close()
  }

  /**
    * read file by byte and returns it as  0...255 Int
    *
    * @return 0...255 Int or -1 if end of stream reached
    */
  override def read(): Int = if (unsignedBytes.nonEmpty) {
    val ret = unsignedBytes.head
    unsignedBytes = unsignedBytes.tail
    ret
  } else {
    if (last > 0) {
      seek()
      pool()
      read()
    } else -1
  }

  private def pool(): Unit = {
    readBuffer.clear()
    channel.read(readBuffer)
    readBuffer.flip()
    unsignedBytes = new ReversUnsignedIntBuffer(readBuffer, readBuffer.limit() - 1)
  }

  private def seek(): Unit = {

    val pos = {
      val c = last - readBuffSize
      if (c > 0) c else 0
    }

    channel.position(pos)
    last = pos

  }
}

object FileReversInputStream {

  def apply(file: File): FileReversInputStream = new FileReversInputStream(file)

  def apply(file: File, readBufSize: Int): FileReversInputStream =
    new FileReversInputStream(file, readBufSize)

  def apply(file: String): FileReversInputStream = apply(new File(file))

  def apply(file: String, readBuffSize: Int): FileReversInputStream =
    apply(new File(file), readBuffSize)

}






