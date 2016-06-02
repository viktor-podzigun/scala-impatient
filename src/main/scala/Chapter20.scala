import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{File, IOException}
import java.util
import javax.imageio.ImageIO
import scala.actors._
import scala.collection.JavaConverters.mapAsScalaMapConverter
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
import scala.io.Source
import scala.util.Random
import scala.util.matching.Regex

//noinspection ScalaDeprecation
object Chapter20 {

  /**
   * Task 1:
   *
   * Write a program that generates an array of `n` random numbers (where `n` is a large value,
   * such as 1,000,000), and then computes the average of those numbers by distributing the work
   * over multiple actors, each of which computes the sum of the values, sending the result to
   * an actor that combines the results.
   * If you run your program on a dual-core or quad-core processor, what is the speedup over
   * a single-threaded solution?
   *
   * Solution:
   *
   * For this simple task, on my machine, single-threaded solution was two times faster then
   * actors solution.
   * Starting actors takes more time then calculating the average of random numbers.
   */
  object RandCalc {

    val MaxWorkers = 10
    val NumPerWorker = 50000

    def calcAverageFor(count: Int, useActors: Boolean): Double = {
      if (useActors) {
        val proc = new RandProcessor()
        proc.start()

        val futureResult = proc.process(count)
        Await.result(futureResult, Duration.Inf)
      }
      else {
        calcAverage(0 until count)
      }
    }

    def calcAverage(range: Range): Double = {
      range.map(_ => Random.nextDouble()).sum / range.size
    }
  }

  case class RandMsgProcess(n: Int)

  case class RandMsgCalc(range: Range)

  case class RandMsgResult(average: Double)

  class RandProcessor() extends Actor {

    protected var workerCount = 0
    private var processedCount = 0
    private var currAverage = 0.0
    private val resultPromise = Promise[Double]()

    def process(count: Int): concurrent.Future[Double] = {
      this ! RandMsgProcess(count)
      resultPromise.future
    }

    def act(): Unit = {
      loop {
        react {
          case RandMsgProcess(num) =>
            startWorkers(num)
          case RandMsgResult(average) =>
            procResult(average)
          case msg =>
            throw new IllegalStateException("Unknown message: " + msg)
        }
      }
    }

    protected def getRanges(untilNum: Int, count: Int): List[Range] = {
      require(untilNum >= count, "untilNum >= count")

      val numPerRange = math.max(untilNum / count, untilNum % count)
      var start = 0
      (for (i <- 1 to count) yield {
        val end = if (i == count) untilNum else start + numPerRange
        val range = start until end
        start = end
        range
      }).toList
    }

    protected def getWorkerCount(numCount: Int): Int = {
      var workerCount = numCount / RandCalc.NumPerWorker
      if (workerCount == 0) workerCount = 1
      else if (workerCount > RandCalc.MaxWorkers) workerCount = RandCalc.MaxWorkers

      workerCount
    }

    private def startWorkers(numCount: Int): Unit = {
      workerCount = getWorkerCount(numCount)

      for (range <- getRanges(numCount, workerCount)) {
        val worker = new RandWorker()
        worker.start()
        worker ! RandMsgCalc(range)
      }
    }

    protected def procResult(average: Double): Unit = {
      processedCount += 1
      currAverage += average

      if (processedCount >= workerCount) {
        resultPromise.success(currAverage / processedCount)
        exit()
      }
    }
  }

  class RandWorker() extends Actor {

    def act(): Unit = {
      react {
        case RandMsgCalc(range) =>
          reply(RandMsgResult(RandCalc.calcAverage(range)))
          exit()
        case msg =>
          throw new IllegalStateException("Unknown message: " + msg)
      }
    }
  }

  /**
   * Task 2:
   *
   * Write a program that reads in a large image into a `BufferedImage`, using
   * `javax.imageio.ImageIO.read`. Use multiple actors, each of which inverts the colors in
   * a strip of the image. When all strips have been inverted, write the result.
   */
  object ImageProgram {

    val MaxWorkers = 10

    def invert(srcFile: File, dstFile: File): Unit = {
      val proc = new ImageProcessor()
      proc.start()

      val futureResult = proc.process(srcFile, dstFile)
      Await.result(futureResult, Duration.Inf)
    }
    
    def loadImage(srcFile: File): BufferedImage = ImageIO.read(srcFile)

    def saveImage(bufferedImage: BufferedImage, dstFile: File): Unit = {
      ImageIO.write(bufferedImage, Utils.getFileExt(dstFile), dstFile)
    }

    def getStrip(image: BufferedImage, stripIndex: Int): Seq[Int] = {
      val strip = new Array[Int](image.getHeight)
      for (y <- 0 until image.getHeight) {
        strip(y) = image.getRGB(stripIndex, y)
      }

      strip.toIndexedSeq
    }

    def setStrip(image: BufferedImage, stripIndex: Int, strip: Seq[Int]): Unit = {
      for (y <- strip.indices) {
        image.setRGB(stripIndex, y, strip(y))
      }
    }

    def invertStrip(strip: Seq[Int]): Seq[Int] = {
      for (rgba <- strip) yield {
        var col = new Color(rgba, true)
        col = new Color(255 - col.getRed, 255 - col.getGreen, 255 - col.getBlue)
        col.getRGB
      }
    }
  }

  case class ImageMsgProcess(srcFile: File, dstFile: File)

  case class ImageMsgInvertStrip(stripIndex: Int, strip: Seq[Int])

  case class ImageMsgInvertResult(stripIndex: Int, invertedStrip: Seq[Int])

  case class ImageMsgProcessEnd()
  
  class ImageProcessor() extends Actor {

    private var stripCount = 0
    private var processedCount = 0
    private var image: BufferedImage = null
    private var resultFile: File = null
    private val resultPromise = Promise[Unit]()

    def process(srcFile: File, dstFile: File): concurrent.Future[Unit] = {
      this ! ImageMsgProcess(srcFile, dstFile)
      resultPromise.future
    }

    def act(): Unit = {
      loop {
        react {
          case ImageMsgProcess(srcFile, dstFile) =>
            procStart(srcFile, dstFile)
          case ImageMsgInvertResult(stripIndex, invertedStrip) =>
            procResult(stripIndex, invertedStrip)
          case msg =>
            throw new IllegalStateException("Unknown message: " + msg)
        }
      }
    }

    private def procStart(srcFile: File, dstFile: File): Unit = {
      image = ImageProgram.loadImage(srcFile)
      stripCount = image.getWidth
      resultFile = dstFile

      // start workers
      val workers = for (i <- 0 until ImageProgram.MaxWorkers) yield {
        val worker = new ImageWorker()
        worker.start()
        worker
      }

      // send invert strip messages to workers
      for (x <- 0 until stripCount) {
        workers(x % workers.length) ! ImageMsgInvertStrip(x, ImageProgram.getStrip(image, x))
      }

      // stop workers
      for (worker <- workers) {
        worker ! ImageMsgProcessEnd
      }
    }
    
    private def procResult(stripIndex: Int, invertedStrip: Seq[Int]): Unit = {
      processedCount += 1
      ImageProgram.setStrip(image, stripIndex, invertedStrip)

      if (processedCount >= stripCount) {
        ImageProgram.saveImage(image, resultFile)

        resultPromise.success(Unit)
        exit()
      }
    }
  }

  class ImageWorker() extends Actor {

    def act(): Unit = {
      loop {
        react {
          case ImageMsgInvertStrip(stripIndex, strip) =>
            reply(ImageMsgInvertResult(stripIndex, ImageProgram.invertStrip(strip)))
          case ImageMsgProcessEnd =>
            exit()
          case msg =>
            throw new IllegalStateException("Unknown message: " + msg)
        }
      }
    }
  }

  /**
   * Task 3:
   *
   * Write a program that counts how many words match a given regular expression in all files of
   * all subdirectories of a given directory. Have one actor per file, one actor that traverses
   * the subdirectories, and one actor to accumulate the results.
   */
  object WordsCountProgram {

    private val wordRegex = "text".r

    def calcMatchedWords(dirPath: String, fileExtensions: String*): Int = {
      val futureResult = new WordsProcessor(
        WordsParams(wordRegex, dirPath, fileExtensions.toIndexedSeq),
        0,
        (count: Int, file, words) => count + words.length
      ).process()

      Await.result(futureResult, Duration.Inf)

//      var count = 0
//      for (file <- Utils.listAllFiles(dirPath, fileExtensions: _*)) {
//        for (line <- Source.fromFile(file).getLines()) {
//          wordRegex.findAllIn(line).foreach(_ => count += 1)
//        }
//      }
//
//      count
    }
  }

  case class WordsParams(wordRegex: Regex, dirPath: String, fileExtensions: Seq[String])

  case class WordsMsgProcess()

  case class WordsMsgProcessEnd()

  case class WordsMsgDir(dirPath: String)

  case class WordsMsgFile(filePath: String)

  case class WordsMsgFileResult(file: File, words: Seq[String])

  class WordsProcessor[T](params: WordsParams,
                          resultInit: T,
                          resultProc: (T, File, Seq[String]) => T) extends Actor {

    protected var processedCount = 0
    private var fileCount = 0
    private var processEnd = false
    private var result: T = resultInit
    private val resultPromise = Promise[T]()

    def process(): concurrent.Future[T] = {
      start()
      this ! WordsMsgProcess()
      resultPromise.future
    }

    def act(): Unit = {
      while (true) {
        receive(processMsg())
      }
    }

    protected def processMsg(): PartialFunction[Any, Unit] = {
      case msg: WordsMsgProcess =>
        startDirWorker(msg)
      case msg: WordsMsgFile =>
        fileCount += 1
        startFileWorker(msg)
      case WordsMsgFileResult(file, words) =>
        processedCount += 1
        result = resultProc(result, file, words)
        checkEnd()
      case _: WordsMsgProcessEnd =>
        processEnd = true
        checkEnd()
      case msg =>
        throw new IllegalStateException("Unknown message: " + msg)
    }

    def checkEnd(): Unit = {
      if (processedCount >= fileCount && processEnd) {
        resultPromise.success(result)
        exit()
      }
    }

    def startDirWorker(msg: WordsMsgProcess): Unit = {
      val dirWorker = new WordsDirWorker(params)
      dirWorker.start()
      dirWorker ! msg
    }

    def startFileWorker(msg: WordsMsgFile): WordsFileWorker = {
      val fileWorker = new WordsFileWorker(params)
      fileWorker.start()
      fileWorker ! msg
      fileWorker
    }
  }

  class WordsDirWorker(params: WordsParams) extends Actor {

    private var processor: OutputChannel[Any] = null
    private var dirCount = 1 // allow process the root directory

    def act(): Unit = {
      while (true) {
        receive {
          case _: WordsMsgProcess =>
            this.processor = sender
            this ! WordsMsgDir(params.dirPath)
          case WordsMsgDir(dirPath) =>
            dirCount -= 1
            processDir(dirPath)
            if (dirCount == 0) {
              processor ! WordsMsgProcessEnd()
              exit()
            }
          case msg =>
            throw new IllegalStateException("Unknown message: " + msg)
        }
      }
    }
    
    def processDir(dirPath: String): Unit = {
      val dir = new File(dirPath)
      require(dir.isDirectory, s"Given path should represent directory: $dirPath")

      val files = dir.listFiles()
      if (files == null) {
        throw new IOException(s"Cannot read directory content: $dir")
      }

      for (file <- files) {
        if (file.isDirectory) {
          // process new directory in this actor
          dirCount += 1
          this ! WordsMsgDir(file.getPath)
        }
        else if (params.fileExtensions.isEmpty ||
          params.fileExtensions.contains(Utils.getFileExt(file))) {

          // notify processor about new file
          processor ! WordsMsgFile(file.getPath)
        }
      }
    }
  }

  class WordsFileWorker(params: WordsParams) extends Actor {

    def act(): Unit = {
      receive {
        case WordsMsgFile(filePath) =>
          processFile(filePath)
          exit()
        case msg =>
          throw new IllegalStateException("Unknown message: " + msg)
      }
    }

    def processFile(filePath: String): Unit = {
      val file = new File(filePath)
      require(file.isFile, s"Given path should represent file: $filePath")

      val words = new ArrayBuffer[String]()
      for (line <- Source.fromFile(file).getLines()) {
        for (word <- params.wordRegex.findAllIn(line)) {
          words += word
        }
      }

      sendResult(file, words.toIndexedSeq)
    }

    def sendResult(file: File, words: Seq[String]): Unit = {
      reply(WordsMsgFileResult(file, words))
    }
  }

  /**
   * Task 4:
   *
   * Modify the program of the preceding exercise to display all matching words.
   */
  object WordsPrintProgram {

    private val wordRegex = "text".r

    def printMatchedWords(dirPath: String, fileExtensions: String*): String = {
      val futureResult = new WordsProcessor(
        WordsParams(wordRegex, dirPath, fileExtensions.toIndexedSeq),
        new StringBuilder(),
        (result: StringBuilder, file, words) => result ++= words.mkString ++= "\n"
      ).process()

      Await.result(futureResult, Duration.Inf).toString()
    }
  }

  /**
   * Task 5:
   *
   * Modify the program of the preceding exercise to display all matching words, each with
   * a list of all files containing it.
   */
  object WordsPrintFilesProgram {

    private val wordRegex = "text".r

    def printMatchedWordsWithFiles(dirPath: String, fileExtensions: String*): String = {
      val futureResult = new WordsProcessor(
        WordsParams(wordRegex, dirPath, fileExtensions.toIndexedSeq),
        new ArrayBuffer[(File, Seq[String])](),
        (result: ArrayBuffer[(File, Seq[String])], file, words) => result += file -> words
      ).process()

      val result: ArrayBuffer[(File, Seq[String])] = Await.result(futureResult, Duration.Inf)

      // map words to files, use sorted map to get stable output
      val wordToFiles = new util.TreeMap[String, List[String]]().asScala
      for ((file, words) <- result; word <- words) {
        val files = wordToFiles.getOrElse(word, Nil)
        wordToFiles(word) = files :+ file.getPath
      }

      // sort the files to get stable output
      for ((word, files) <- wordToFiles) {
        wordToFiles(word) = files.sorted
      }

      // get the output
      val out = new StringBuilder()
      for ((word, files) <- wordToFiles) {
        out ++= "found \"" ++= word ++= "\" in\n" ++= files.mkString("\n") ++= "\n\n"
      }

      out.toString()
    }
  }

  /**
   * Task 6:
   *
   * Write a program that constructs 100 actors that use a `while(true)/receive` loop,
   * calling `println(Thread.currentThread)` when they receive a 'Hello message,
   * and 100 actors that do the same with `loop/react`. Start them all,
   * and send them all a message.
   * How many threads are occupied by the first kind, and how many by the second kind?
   *
   * Solution:
   *
   * The result is impressive, the numbers are the following:
   * {{{
   * whileReceiveActorsCount: 100
   * loopReactActorsCount: 1
   * }}}
   */
  object ThreadActorsProgram {

    val actorsCount = 100

    def calcActorThreads(whileReceiveActors: Boolean): Int = {
      val futureResult = new ThreadActorsProcessor(whileReceiveActors).process()
      Await.result(futureResult, Duration.Inf)
    }
  }

  case class ThreadMsgProcess()
  
  case class ThreadMsgHello()

  case class ThreadMsgThreadId(threadId: Long)

  class ThreadActorsProcessor(whileReceiveActors: Boolean) extends Actor {

    private val threadIds = mutable.HashSet[Long]()
    private var processedCount = 0
    private val resultPromise = Promise[Int]()

    def process(): concurrent.Future[Int] = {
      start()
      this ! ThreadMsgProcess()
      resultPromise.future
    }

    override def act(): Unit = {
      loop {
        react {
          case _: ThreadMsgProcess =>
            startActors()
          case ThreadMsgThreadId(threadId) =>
            procResult(threadId)
        }
      }
    }
    
    def startActors(): Unit = {
      for (_ <- 0 until ThreadActorsProgram.actorsCount) {
        val actor =
          if (whileReceiveActors) new ThreadWhileReceiveActor()
          else new ThreadLoopReactActor()

        actor.start()
        actor ! ThreadMsgHello()
      }
    }
    
    def procResult(threadId: Long): Unit = {
      threadIds += threadId
      processedCount += 1
      
      if (processedCount >= ThreadActorsProgram.actorsCount) {
        resultPromise.success(threadIds.size)
        exit()
      }
    }
  }

  class ThreadWhileReceiveActor extends Actor {

    override def act(): Unit = {
      while (true) {
        receive {
          case _: ThreadMsgHello =>
            //println(Thread.currentThread)
            reply(ThreadMsgThreadId(Thread.currentThread().getId))
        }
      }
    }
  }

  class ThreadLoopReactActor extends Actor {

    override def act(): Unit = {
      loop {
        react {
          case _: ThreadMsgHello =>
            //println(Thread.currentThread)
            reply(ThreadMsgThreadId(Thread.currentThread().getId))
        }
      }
    }
  }

  /**
   * Task 7:
   *
   * Add a supervisor to the program of exercise 3 that monitors the file reading actors
   * and logs any that exit with an `IOException`. Try triggering the exception by removing
   * files that have been scheduled for processing.
   */
  object WordsSupervisorProgram {

    private val wordRegex = "text".r

    def calcMatchedWords(dirPath: String, fileExtensions: String*): Int = {
      val supervisorProcessor = new WordsSupervisorProcessor(
        WordsParams(wordRegex, dirPath, fileExtensions.toIndexedSeq),
        0,
        (count: Int, file, words) => count + words.length
      )

      // run process and wait for result
      val result = Await.result(supervisorProcessor.process(), Duration.Inf)

      // get possible errors and log them on main thread
      println(supervisorProcessor.getErrors)
      result
    }
  }

  class WordsSupervisorProcessor[T](params: WordsParams,
                                    resultInit: T,
                                    resultProc: (T, File, Seq[String]) => T
                                     ) extends WordsProcessor[T](params, resultInit, resultProc) {

    private val errors = new StringBuilder()

    def getErrors: String = errors.toString()

    override protected def processMsg(): PartialFunction[Any, Unit] = {
      case msg: WordsMsgProcess =>
        trapExit = true
        super.processMsg().apply(msg)
      case Exit(linked, UncaughtException(_, _, _, _, cause)) =>
        processedCount += 1
        errors.append(cause) ++= "\n"
        checkEnd()
      case Exit(linked, reason) =>
        if (reason != 'normal) {
          processedCount += 1
          errors.append(reason) ++= "\n"
          checkEnd()
        }
      case msg =>
        super.processMsg().apply(msg)
    }

    override def startFileWorker(msg: WordsMsgFile): WordsFileWorker = {
      val fileWorker = if (msg.filePath.endsWith(".txt"))
        super.startFileWorker(WordsMsgFile(msg.filePath + "-not-exist"))
      else
        super.startFileWorker(msg)

      link(fileWorker)
      fileWorker
    }
  }

  /**
   * Task 8:
   *
   * Show how an actor-based program can deadlock when one sends synchronous messages.
   */
  object DeadlockProgram {

    val seconds = 3

    def run(): Int = {
      val futureResult = new DeadlockProcessor().process()
      Await.result(futureResult, Duration.Inf)
    }
  }

  case class DeadlockMsgProcess()

  case class DeadlockMsgSync()

  case class DeadlockMsgResult(result: Int)

  class DeadlockProcessor extends Actor {

    private val resultPromise = Promise[Int]()

    def process(): concurrent.Future[Int] = {
      start()
      this ! DeadlockMsgProcess()
      resultPromise.future
    }

    override def act(): Unit = {
      loop {
        react {
          case _: DeadlockMsgProcess =>
            val actor = new DeadlockActor(this)
            actor.start()
            // send synchronous message to actor
            actor !? DeadlockMsgSync() match {
              case DeadlockMsgResult(result) =>
                resultPromise.success(result)
                exit()
            }
          case _: DeadlockMsgSync =>
            reply(DeadlockMsgResult(5))
        }
      }
    }
  }

  class DeadlockActor(parent: Actor) extends Actor {

    override def act(): Unit = {
      loop {
        react {
          case _: DeadlockMsgSync =>
            // ask parent actor for data synchronously and wait some time
            val optionResult = parent !? (DeadlockProgram.seconds * 1000L, DeadlockMsgSync())
            if (optionResult.isDefined) {
              optionResult.get match {
                case DeadlockMsgResult(result) =>
                  reply(DeadlockMsgResult(result * 10))
              }
            }
            else {
              // timeout, no data
              reply(DeadlockMsgResult(-1))
            }
        }
      }
    }
  }

  /**
   * Task 9:
   *
   * Produce a faulty implementation of the program in exercise 3, in which the actors update
   * a shared counter. Can you demonstrate that the program acts incorrectly?
   */
  object SharedCounterProgram {

    private val wordRegex = "text".r
    var counter = 0

    def calcMatchedWords(dirPath: String, fileExtensions: String*): Int = {
      val supervisorProcessor = new SharedCounterProcessor(
        WordsParams(wordRegex, dirPath, fileExtensions.toIndexedSeq),
        0,
        (count: Int, file, words) => count + words.length
      )

      Await.result(supervisorProcessor.process(), Duration.Inf)
    }
  }

  class SharedCounterProcessor[T](params: WordsParams,
                                  resultInit: T,
                                  resultProc: (T, File, Seq[String]) => T
                                   ) extends WordsProcessor[T](params, resultInit, resultProc) {

    override def startFileWorker(msg: WordsMsgFile): WordsFileWorker = {
      val fileWorker = new SharedCounterFileWorker(params)
      fileWorker.start()
      fileWorker ! msg
      fileWorker
    }
  }

  class SharedCounterFileWorker(params: WordsParams) extends WordsFileWorker(params) {

    override def sendResult(file: File, words: Seq[String]): Unit = {
      val counter = SharedCounterProgram.counter

      // introduce some delay to allow other threads update the counter
      Thread.sleep(200)

      //NOT SAFE: updating shared counter from multiple threads !!!
      SharedCounterProgram.counter = counter + words.length

      super.sendResult(file: File, words: Seq[String])
    }
  }

  /**
   * Task 10:
   *
   * Rewrite the program of exercise 1 by using channels for communication.
   */
  object ChannelCalc {

    def calcAverageFor(count: Int): Double = {
      val proc = new ChannelProcessor()
      proc.start()

      val futureResult = proc.process(count)
      Await.result(futureResult, Duration.Inf)
    }
  }

  class ChannelProcessor() extends RandProcessor {

    override def act(): Unit = {
      react {
        case RandMsgProcess(num) =>
          doProcess(num)
      }
    }

    private def doProcess(numCount: Int): Unit = {
      workerCount = getWorkerCount(numCount)

      val resultChannel = new Channel[Double]

      // start actors
      for (range <- getRanges(numCount, workerCount)) {
        Actor.actor {
          val channel = new Channel[Range]
          channel ! range
          channel.react {
            case r =>
              resultChannel ! RandCalc.calcAverage(r)
          }
        }
      }

      // process results from the actors
      loop {
        resultChannel.react {
          case average =>
            procResult(average)
        }
      }
    }
  }
}
