package parallelprog.ch2

import scala.collection.mutable

object WorkerApp3 extends App {

  private val tasks = mutable.Queue[() => Unit]()

  object Worker extends Thread {
    setDaemon(true)

    def poll(): () => Unit = tasks.synchronized {
      while (tasks.isEmpty) tasks.wait()
      tasks.dequeue()
    }

    override def run(): Unit = while (true) {
      val task = poll()
      task()
    }
  }

  Worker.start()
  

  def asynchronous(body: => Unit): Unit = tasks.synchronized {
    tasks.enqueue(() => body)
    tasks.notify()
  }

  asynchronous {
    println("Hello ")
  }
  asynchronous {
    println("World!")
  }
  
  Thread.sleep(500)

}
