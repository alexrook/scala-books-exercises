package horstmann.io.revers

import java.nio.ByteBuffer

trait Revers[+A] {
  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

  def head: A

  def tail: Revers[A]
}

object EmptyRevers$ extends Revers[Nothing] {

  override def isEmpty: Boolean = true

  override def head: Nothing =
    throw new NoSuchElementException("head of empty revert")

  override def tail: Revers[Nothing] =
    throw new UnsupportedOperationException("tail of empty revert")
}

class ReversBuffer[A](buffer: ByteBuffer, pos: Int)
  extends Revers[A] {

  override def isEmpty: Boolean = false || pos < 0

  override def head: A = {
    val ret = buffer.get(pos).asInstanceOf[A]
    ret
  }

  override def tail =
    if (pos >= 0) new ReversBuffer[A](buffer, pos - 1)
    else EmptyRevers$
}

class ReversUnsignedIntBuffer(buffer: ByteBuffer, pos: Int)
  extends Revers[Int] {
  override def isEmpty = false || pos < 0

  override def head: Int = {
    val ret = buffer.get(pos) & 0xff
    ret
  }

  override def tail =
    if (pos >= 0) new ReversUnsignedIntBuffer(buffer, pos - 1)
    else EmptyRevers$
}

