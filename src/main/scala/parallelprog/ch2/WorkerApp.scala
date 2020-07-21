package parallelprog.ch2

import scala.collection.mutable

object WorkerApp extends App {

  object Tasks {
    val tasks: mutable.Queue[() => Unit] = mutable.Queue.empty[() => Unit]

    def add(work: => Unit): Unit = tasks.synchronized {
      tasks.enqueue(() => work)
    }

    def pool(): Option[() => Unit] = tasks.synchronized {
      if (tasks.nonEmpty)
        Some(tasks.dequeue())
      else {
        None
      }
    }
  }

  object Worker extends Thread {
    override def run(): Unit = while (true) {
      Tasks.pool() match {
        case Some(work) =>
          print("In thread ")
          work()
        case None =>
      }
    }
  }

  Worker.setName("Worker")
  Worker.setDaemon(true)
  Worker.start()

  Tasks.add(println("Work1"))
  Tasks.add(println("Work2"))

  Thread.sleep(3000)

}
