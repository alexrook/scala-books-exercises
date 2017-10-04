package horstmann

object Section10 extends App {

  val resourcesDir = "out/production/resources"

  object l10 {

    object l101 {

      trait MyTrait {

        def dom(arg: String): Unit = {
          /*nothing here*/
        }

      }

      class Worker extends MyTrait {

        def work(arg: String): Unit = {
          println("Worker say:" + arg)
          dom(arg)
        }
      }

      trait MyTraitExt extends MyTrait {
        override def dom(arg: String): Unit = {
          println("MyTraitExt say:" + arg)
        }
      }

      val worker1 = new Worker
      worker1.work("w1")
      //trait mixin
      val worker2 = new Worker with MyTraitExt
      worker2.work("w2")

    }

    object l102 {

      trait Logger {

        def log(msg: String) = {}

      }

      trait ConsoleLogger extends Logger {
        override def log(msg: String): Unit = println(msg)
      }

      trait TimeStampLogger extends Logger {
        override def log(msg: String): Unit = super.log(new java.util.Date() + ":" + msg)
      }

      trait ShortLogger extends Logger {
        override def log(msg: String): Unit = super.log(msg.take(12) + "...")
      }

      class WorkerE extends Logger {

        def work(arg: String): Unit = {
          println("Worker say:" + arg)
          log(arg)
        }

      }

      //super вызывает предыдущие логгеры в списке ShortLogger
      // укорачивание -> добавление даты -> вывод на консоль
      val worker1 = new WorkerE with ConsoleLogger with TimeStampLogger with ShortLogger
      worker1.work("w1:12345678901234567890")

      //super вызывает предыдущие логгеры в списке TimeStampLogger
      // добавление даты -> укорачивание -> вывод на консоль
      val worker2 = new WorkerE with ConsoleLogger with ShortLogger with TimeStampLogger

      worker2.work("w2:12345678901234567890")
    }

    object l103 {

      trait Base {
        def one(arg: Int): Int
      }

      trait BaseExt extends Base {
        /*note abstract here - because super.one */
        abstract override def one(arg: Int): Int = super.one(arg + 1)
      }

    }

    object l104 {

      trait Logger {
        println("Logger constructor")

        def log(msg: String)

        def warn(msg: String) = log(s"WARN:$msg")

        def info(msg: String) = log(s"INFO:$msg")
      }

      trait ConsoleLogger extends Logger {
        println("ConsoleLogger constructor")

        override def log(msg: String): Unit = println(msg)
      }

      abstract class Worker extends Logger {
        println("Worker constructor")

        def work(arg: String): Unit = {
          println("Worker say:" + arg)
          info(arg)
        }
      }

      trait TimeStampLogger extends Logger {
        println("TimeStampLogger constructor")

        abstract override def log(msg: String): Unit = super.log(new java.util.Date() + ":" + msg)
      }

      trait ShortLogger extends Logger {
        println("ShortLogger constructor")

        abstract override def log(msg: String): Unit = super.log(msg.take(12) + "...")
      }

      val worker1 = new Worker with ConsoleLogger
      worker1.work("l104:w1")

      println("---")
      //lin(worker2)=lin(Anonymous)>>lin(ShortLogger)>>lin(TimeStampLogger)>>lin(ConsoleLogger)>>lin(Worker)=
      //(Anonymous)>>(ShortLogger>>Logger)
      //      >>(TimeStampLogger>>Logger)>>(ConsoleLogger>>Logger)>>lin(Worker>>Logger)=
      //Anonymous>>ShortLogger>>TimeStampLogger>>ConsoleLogger>>Worker>>Logger
      //Logger>>Worker>>ConsoleLogger>>TimeStampLogger>>ShortLogger>>Anonymous
      val worker2 = new Worker with ConsoleLogger with TimeStampLogger with ShortLogger
      worker2.work("l104:w2")

      //Линеаризация класса
      //lin(WorkerExt)=WorkerExt>>lin(ShortLogger)>>lin(TimeStampLogger)>>lin(ConsoleLogger)>>lin(Worker)=
      //WorkerExt>>(ShortLogger>>Logger)>>lin(TimeStampLogger>>Logger)>>lin(ConsoleLogger>>Logger)>>(Worker>>Logger)=
      //WorkerExt>>ShortLogger>>TimeStampLogger>>ConsoleLogger>>Worker>>Logger
      //конструкторы выполнятся в обратном порядке
      //Logger>>Worker>>ConsoleLogger>>TimeStampLogger>>ShortLogger>>WorkerExt

      class WorkerExt extends Worker with
        ConsoleLogger with TimeStampLogger with ShortLogger {
        println("WorkerExt constructor")
      }

      val workerExt = new WorkerExt

      workerExt.work("l104:wext")

      //lin(forward)=lin(Anonymous)>>lin(ConsoleLogger)>>lin(Worker)>>lin(Anonymous block)=
      //Anonymous>>(ConsoleLogger>>Logger)>>(Worker>>Logger)>>(Anonymous block)=
      //Anonymous>>ConsoleLogger>>Worker>>Logger>>(Anonymous block)
      //constructor chain: (Anonymous block)>>Logger>>Worker>>ConsoleLogger>>Anonymous
      val forward = new {
        //Anonymous block
        val some = "some"
      } with Worker with ConsoleLogger

      forward.work(forward.some)
      println(forward.getClass.getName)
    }

    object l105 {

      class Base {
        println("Base Constructor")
        var i: Int = _

        def get(): Int = {
          i += 1
          i
        }
      }

      trait Logger {
        println("Logger constructor")

        def log(msg: String)
      }

      trait ConsoleLogger extends Base with Logger {
        println("ConsoleLogger constructor")

        override def log(msg: String): Unit = println(get() + ":" + msg)
      }

      abstract class WorkerBase extends Logger {
        def work(msg: String) = {
          println("workerbase:" + msg)
          log(msg)
        }
      }

      //      Illegal inheritance; superclass WorkerBase
      //      is not a subclass of the superclass Base
      //        of the mixin trait ConsoleLogger
      //      class Worker extends WorkerBase with ConsoleLogger {
      //      class Worker extends WorkerBase with ConsoleLogger {
      //        override def work(msg: String): Unit = super.work("worker>>" + msg)
      //      }

      //lin(Worker)=Worker>>lin(ConsoleLogger)=
      //Worker>>ConsoleLogger>>Logger>>Base
      class Worker extends ConsoleLogger {
        println("Worker constructor")

        def work(msg: String) = {
          println(msg)
          log(msg)
        }
      }

      val worker = new Worker
      worker.work("l105:w1")

      val log = new ConsoleLogger {}
      log.log(s"l105:${log.getClass.getName}")
    }

    object l106 {

      class Worker {
        println("Worker constructor")

        def work(msg: String) = println("Worker:" + msg)
      }

      trait Logger {
        this: Worker =>
        println("Logger constructor")

        def log(msg: String) = work("Logger:" + msg)
      }

      /*
      *Illegal inheritance;
       self-type WrongClz does not conform to Logger's
       selftype Logger with Worker*/
      //  class WrongClz extends Logger

      //lin(WorkerEx)=WorkerEx>>lin(Logger)>>lin(Worker)=
      //WorkerEx>>Logger>>Worker
      class WorkerEx extends Worker with Logger {
        println("WorkerEx constructor")
      }

      val workerEx = new WorkerEx
      workerEx.log("l106:we1")

    }

    object l107 {

      //циклический структурный тип Section18
      trait Logger {
        this: {def getDate: String} =>
        def log(msg: String) = println(getDate + ":" + msg)
      }

      class Worker extends Logger {
        //Worker must conform {def getDate: String}
        def getDate: String = new java.util.Date().toString
      }

      val worker = new Worker
      worker.log("l107:w1")
    }

    l101
    l102
    l103
    l104
    l105
    l106
    l107
  }

  //l10

  object q1 {

    import java.awt.Rectangle

    import java.awt.geom.Ellipse2D

    trait RectangleLike {

      def getX: Double

      def getY: Double

      def getWidth: Double

      def getHeight: Double

      def setFrame(x: Double, y: Double, w: Double, h: Double)

      def translate(dx: Int, dy: Int) = {
        val rect = new Rectangle(getX.toInt, getY.toInt,
          getWidth.toInt, getHeight.toInt)

        rect.translate(dx, dy)

        setFrame(rect.x, rect.y, rect.width, rect.height)

      }

      def grow(h: Int, v: Int) = {
        val rect = new Rectangle(getX.toInt, getY.toInt,
          getWidth.toInt, getHeight.toInt)

        rect.grow(h, v)

        setFrame(rect.x, rect.y, rect.width, rect.height)
      }

    }

    val egg = new Ellipse2D.Double(5, 10, 20, 30) with RectangleLike
    egg.translate(10, -10)
    egg.grow(10, 20)
  }

  object q2 {

    import java.awt.Point
    import scala.math.Ordered

    class OrderedPoint(x: Int, y: Int) extends Point(x, y) with Ordered[Point] {
      override def compare(that: Point): Int = if ((this.x < that.x) || ((this.x == that.x) && (this.y < that.y))) -1
      else if ((this.x == that.x) && (this.y == that.y)) 0
      else 1
    }

    val p1 = new OrderedPoint(1, 2)
    val p2 = new OrderedPoint(2, 2)
    val p3 = new OrderedPoint(1, 2)
    val p4 = new OrderedPoint(2, 1)

    println(p1.compare(p2))
    println(p1.compare(p3))
    println(p1.compare(p4))
    println(p2.compare(p4))

    val x1 = new OrderedPoint(1, 1)
    val x2 = new OrderedPoint(1, -1)
    val x3 = new OrderedPoint(2, 1)

    println(x1 < x2)
    println(x1 > x2)
    println(x1 >= x3)

  }

  object q3 {
    //trait BitSet extends SortedSet with BitSetLike //todo: lin(BitSet) == ???
  }

  object q4 {
    println("-----------------------")

    trait Logger {
      //  println("Logger constr")

      def log(msg: String): String
    }

    trait ConsoleLogger extends Logger {
      //println("ConsoleLogger constr")

      def log(msg: String): String = {
        println(msg)
        msg
      }
    }

    trait FileLogger extends Logger {

      import java.io._

      val logFile = File.createTempFile("section10-", ".log")
      val out = new FileOutputStream(logFile)

      def log(msg: String): String = {
        out.write(msg.getBytes)
        out.flush()
        msg
      }

      def close() = out.close()
    }

    //    val wrlogger = new ConsoleLogger with FileLogger
    //
    //    wrlogger.log("section10:wrlogger:log11")
    //    wrlogger.close()

    trait CryptoLogger extends Logger {
      // println("CryptoLogger constr")
      val chipper = 3
      val CHAR_MAX_VAL: Char = (math.pow(2, 16) - 1).toChar

      abstract override def log(msg: String): String = super.log(encode(msg))

      def decode(msg: String) = msg.map((char) => {
        val ret = char - chipper
        if (ret < 0) {
          (ret + CHAR_MAX_VAL).toChar
        } else ret.toChar
      })

      def encode(msg: String) = msg.map((char) => {
        val ret = char + chipper
        if (ret > CHAR_MAX_VAL) {
          (ret - CHAR_MAX_VAL).toChar
        } else ret.toChar
      })

    }

    //lin(logger)=(Anonymous)>>lin(CryptoLogger)>>lin(ConsoleLogger)=
    //Anonymous>>(CryptoLogger>>Logger)>>(ConsoleLogger>>Logger)=
    //Anonymous>>CryptoLogger>>ConsoleLogger>>Logger
    val logger = new ConsoleLogger with CryptoLogger
    val encoded = logger.encode("Hello World")
    val logged = logger.log("Hello World")
    val decoded = logger.decode(encoded)
    println("Hello World: " + logged + " : " + encoded + " : " + decoded)

    //lin(wrongLogger)=(Anonymous)>>lin(ConsoleLogger)>>lin(CryptoLogger)=
    //Anonymous>>(ConsoleLogger>>Logger)>>(CryptoLogger>>Logger)=
    //Anonymous>>ConsoleLogger>>CryptoLogger>>Logger
    //  val wrongLogger=new CryptoLogger with ConsoleLogger ???

    val logger12 = new {
      override val chipper = 12
    } with ConsoleLogger with CryptoLogger
    val encoded12 = logger12.encode("Hello World")
    val decoded12 = logger12.decode(encoded12)
    println("Hello World: " + encoded12 + " : " + decoded12)

  }

  object q5 {

    import java.beans.{PropertyChangeSupport, PropertyChangeEvent, PropertyChangeListener}

    trait PropertyChangeSupportLike {

      def source: AnyRef

      private val pcs = new PropertyChangeSupport(source)

      //thank you Idea for delegate methods. I don't know how to implement q5 by another way
      def addPropertyChangeListener(propertyName: String, listener: PropertyChangeListener): Unit =
        pcs.addPropertyChangeListener(propertyName, listener)

      def fireIndexedPropertyChange(propertyName: String, index: Int, oldValue: scala.Any, newValue: scala.Any): Unit =
        pcs.fireIndexedPropertyChange(propertyName, index, oldValue, newValue)

      def fireIndexedPropertyChange(propertyName: String, index: Int, oldValue: Boolean, newValue: Boolean): Unit =
        pcs.fireIndexedPropertyChange(propertyName, index, oldValue, newValue)

      def firePropertyChange(event: PropertyChangeEvent): Unit =
        pcs.firePropertyChange(event)

      def hasListeners(propertyName: String): Boolean =
        pcs.hasListeners(propertyName)

      def firePropertyChange(propertyName: String, oldValue: Int, newValue: Int): Unit =
        pcs.firePropertyChange(propertyName, oldValue, newValue)

      def firePropertyChange(propertyName: String, oldValue: scala.Any, newValue: scala.Any): Unit =
        pcs.firePropertyChange(propertyName, oldValue, newValue)

      def firePropertyChange(propertyName: String, oldValue: Boolean, newValue: Boolean): Unit =
        pcs.firePropertyChange(propertyName, oldValue, newValue)

      def fireIndexedPropertyChange(propertyName: String, index: Int, oldValue: Int, newValue: Int): Unit =
        pcs.fireIndexedPropertyChange(propertyName, index, oldValue, newValue)

      def getPropertyChangeListeners: Array[PropertyChangeListener] = pcs.getPropertyChangeListeners

      def removePropertyChangeListener(propertyName: String, listener: PropertyChangeListener): Unit =
        pcs.removePropertyChangeListener(propertyName, listener)

      def getPropertyChangeListeners(propertyName: String): Array[PropertyChangeListener] =
        pcs.getPropertyChangeListeners(propertyName)

      def addPropertyChangeListener(listener: PropertyChangeListener): Unit =
        pcs.addPropertyChangeListener(listener)

      def removePropertyChangeListener(listener: PropertyChangeListener): Unit =
        pcs.removePropertyChangeListener(listener)
    }

    //lin(Worker)=Worker>>lin(PropertyChangeSupportLike)=Worker>>PropertyChangeSupportLike
    //constructor chain=PropertyChangeSupportLike->Worker
    class Worker(private var _a: String) extends PropertyChangeSupportLike {

      override def source = this

      def a = _a

      def a_=(v: String): Unit = {
        firePropertyChange("a", _a, v)
        this._a = v
      }

      override def toString: String = "Worker"
    }

    val worker = new Worker("A")
    worker.addPropertyChangeListener("a",
      (evt: PropertyChangeEvent) => println(evt.getSource + "." +
        evt.getPropertyName + ":" + evt.getOldValue +
        "=>" + evt.getNewValue))
    worker.a = "B"
    worker.a = "C"

    import java.awt.Point

    class PointExt(_x: Int, _y: Int) extends Point(_x, _y) with PropertyChangeSupportLike {
      override def source: AnyRef = this

      private def firedChangeDec[T <: AnyVal](x: T, y: T, f: (T, T) => Unit) = {
        firePropertyChange("x", this.x, x)
        firePropertyChange("y", this.y, y)
        f(x, y)
      }

      override def move(x: Int, y: Int): Unit = firedChangeDec(x, y, super.move)

      override def setLocation(x: Double, y: Double) =
        firedChangeDec(x, y, (x: Double, y: Double) => super.setLocation(x, y))

      override def translate(dx: Int, dy: Int): Unit = firedChangeDec(x, y, super.translate)
    }

    val point = new PointExt(1, 1)

    point.addPropertyChangeListener("x", (evt: PropertyChangeEvent) => println(evt.getSource + "." +
      evt.getPropertyName + ":" + evt.getOldValue +
      "=>" + evt.getNewValue))

    point.addPropertyChangeListener("y", (evt: PropertyChangeEvent) => println(evt.getSource + "." +
      evt.getPropertyName + ":" + evt.getOldValue +
      "=>" + evt.getNewValue))

    point.move(2, 2)
    point.x = 12
    println(point)

  }

  object q7 {
    //todo
  }

  object q8 {

    import java.io._

    trait BufferedInputStreamLike {

      this: InputStream =>

      val bis = new BufferedInputStream(this)

      override def read(): Int = {
        val ret = bis.read()
        //   System.err.println("buf read:" + ret)
        ret
      }
    }

    //lin(io)=Anonymous>>lin(BufferedInputStreamLike)>>lin(FileInputStream)=
    //Anonymous>>BufferedInputStreamLike>>FileInputStream>>InputStream=
    //cc=InputStream->FileInputStream->BufferedInputStreamLike->Anonymous
    val io = new FileInputStream(new File(s"$resourcesDir/section10.q8.data")) with BufferedInputStreamLike

    var byte = 0
    do {
      byte = io.read()
      if (byte > -1) print(byte.toChar)

    } while (byte > -1)
    io.close()

  }

  object q9 {

    import java.io._

    trait Logger {
      def log(msg: String)
    }

    trait StdErrLogger extends Logger {
      override def log(msg: String): Unit = System.err.println(msg)
    }

    trait FileLogger extends Logger {

      def logFileName: String

      val out = new PrintWriter(new FileOutputStream(logFileName))

      override def log(msg: String): Unit = out.println(msg)

      def closeLog() = {
        out.flush()
        out.close()
      }
    }

    trait BufferedInputStreamLike extends Logger {

      this: InputStream =>

      val bis = new BufferedInputStream(this)

      override def read(): Int = {
        val ret = bis.read()
        log("buf read:" + ret)
        ret
      }
    }

    val io = new {
      val logFileName = s"$resourcesDir/section10.q9.log"
    } with FileInputStream(new File(s"$resourcesDir/section10.q8.data"))
      with BufferedInputStreamLike with FileLogger

    var byte = 0
    do {
      byte = io.read()
      if (byte > -1) print(byte.toChar)

    } while (byte > -1)
    io.closeLog()
    io.close()

    import scala.io.Source

    val log = Source.fromFile(s"$resourcesDir/section10.q9.log")
    for (line <- log.getLines()) println(line)
    log.close()

  }

  object q10 {

    import java.io.{InputStream, FileInputStream}

    trait IterableInputStream extends InputStream with Iterable[Byte] {

      outer =>

      override def iterator: Iterator[Byte] = new Iterator[Byte] {

        var nextByte = outer.read()

        override def hasNext: Boolean = nextByte > -1

        override def next(): Byte = {
          val ret = nextByte
          nextByte = outer.read()
          ret.toByte
        }
      }

    }

    val src = new FileInputStream(s"$resourcesDir/section10.q10.data")
      with IterableInputStream

    for (byte <- src) print(byte.toChar)

  }

  //  q1
  //  q2
  //  q4
  //  q5
  //  q8
  //  q9
  q10
}
