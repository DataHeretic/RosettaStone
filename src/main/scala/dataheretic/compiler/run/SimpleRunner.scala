package dataheretic.compiler.run

import better.files._


object SimpleRunner extends App {
  val dhRootDir = File(args(0))
  val dhTarget = File(args(1))
  val watchDir = args.contains("--watch")
  val watchFrequencyMs = args.find(_.contains("--watch_freq")) map { fArg =>
    fArg.split("=")(1) toInt
  } getOrElse 5000

  val writer = new Writer(dhTarget)

  println(
    """|▄▄▄      .▄▄ ·▄▄▄ ▄▄▄▄▄▄▄▄▄▄▄▄·     .▄▄ ▄▄▄▄▄     ▐ ▄▄▄▄ .
       |▀▄ █▪    ▐█ ▀.▀▄.▀•██ •██ ▐█ ▀█     ▐█ ▀•██ ▪    •█▌▐▀▄.▀·
       |▐▀▀▄ ▄█▀▄▄▀▀▀█▐▀▀▪▄▐█.▪▐█.▄█▀▀█     ▄▀▀▀█▐█.▪▄█▀▄▐█▐▐▐▀▀▪▄
       |▐█•█▐█▌.▐▐█▄▪▐▐█▄▄▌▐█▌·▐█▌▐█ ▪▐▌    ▐█▄▪▐▐█▌▐█▌.▐██▐█▐█▄▄▌
       |.▀  ▀▀█▄▀▪▀▀▀▀ ▀▀▀ ▀▀▀ ▀▀▀ ▀  ▀      ▀▀▀▀▀▀▀ ▀█▄▀▀▀ █▪▀▀▀  """.stripMargin)

  if (watchDir)
    new Watcher(dhRootDir, writer, watchFrequencyMs).runForever()
  else
    writer.writeToFile(new Project(dhRootDir).verifyAndGet)
}
