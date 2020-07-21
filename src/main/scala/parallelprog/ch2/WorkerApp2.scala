package parallelprog.ch2

import scala.collection.mutable

object WorkerApp2 extends App {

  val tasks: mutable.Queue[() => Unit] = mutable.Queue.empty[() => Unit]


  object Worker extends Thread {
    override def run(): Unit = tasks.synchronized(
      doWork()
    )

    private def doWork(): Unit = {
      if (tasks.nonEmpty) {
        print("in worker ")
        tasks.dequeue()()
        doWork()
      } else {
        tasks.wait()
        doWork()
      }
    }

  }

  Worker.setName("Worker")
  Worker.setDaemon(true)
  Worker.start()

  // Thread.sleep(3000)

  tasks.enqueue(() => println("Work1"))
  tasks.enqueue(() => println("Work2"))
  tasks.synchronized(
    tasks.notify()
  )


  Thread.sleep(500)

}
