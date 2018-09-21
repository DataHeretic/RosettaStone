package dataheretic.compiler.run

import java.nio.file.WatchEvent
import java.util.concurrent.atomic.AtomicBoolean

import better.files.{File, FileMonitor}

class Watcher (dhRootDir: File, writer: Writer, watchFrequencyMs: Int) {

  private val toggle = new AtomicBoolean(false)

  def runForever(): Unit = {
    println(s"> (file watcher) watching root directory: $dhRootDir")
    watcher.start()(scala.concurrent.ExecutionContext.global)

    while (true) {
      Thread.sleep(watchFrequencyMs)
      if (toggle.get()) {
        toggle.set(false)
        writer.writeToFile(new Project(dhRootDir).verifyAndGet)
      }
    }
  }

  private val watcher = new FileMonitor(dhRootDir, recursive = true) {
    override def onDelete(file: File, count: Int): Unit =
      if (file != writer.dhTarget) {
        println (s"> (file watcher) deleted:  $file")
        toggle.set(true)
      }

    override def onModify(file: File, count: Int): Unit =
      if (file != writer.dhTarget) {
        println (s"> (file watcher) modified: $file")
        toggle.set(true)
      }

    override def onCreate(file: File, count: Int): Unit =
      if (file != writer.dhTarget) {
        println (s"> (file watcher) created:  $file")
        toggle.set(true)
      }

    override def onUnknownEvent(event: WatchEvent[_]): Unit =
      println (s"> (file watcher) unknown event: ${event}")

    override def onException(exception: Throwable): Unit = {
      System.err.println("> (file watcher) unexpected error:")
      exception.printStackTrace(System.err)
    }
  }
}
