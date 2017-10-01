package horstmann

import horstmann.Section10.l10.l104.{ConsoleLogger, ShortLogger, TimeStampLogger, Worker}

object Section10 extends App {

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
      val forward = new {//Anonymous block
        val some = "some"
      } with Worker with ConsoleLogger

      forward.work(forward.some)
      println(forward.getClass.getName)
    }

    //l101
    l102
    l103
    l104
  }

  l10
}
