import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{File, IOException}
import javax.imageio.ImageIO
import scala.actors.{Actor, OutputChannel}
import scala.collection.immutable.Seq
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
import scala.io.Source
import scala.util.Random

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

    private var workerCount = 0
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

    private def getRanges(untilNum: Int, count: Int): List[Range] = {
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

    private def startWorkers(numCount: Int): Unit = {
      workerCount = numCount / RandCalc.NumPerWorker
      if (workerCount == 0) workerCount = 1
      else if (workerCount > RandCalc.MaxWorkers) workerCount = RandCalc.MaxWorkers

      for (range <- getRanges(numCount, workerCount)) {
        val worker = new RandWorker()
        worker.start()
        worker ! RandMsgCalc(range)
      }
    }

    private def procResult(average: Double): Unit = {
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
  object WordsProgram {

    val wordRegex = "text".r

    def calcMatchedWords(dirPath: String, fileExtensions: String*): Int = {
      val futureResult = new WordsProcessor().process(dirPath, fileExtensions: _*)
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

  case class WordsMsgProcess(dirPath: String, fileExtensions: String*)

  case class WordsMsgProcessEnd()

  case class WordsMsgDir(dirPath: String)

  case class WordsMsgFile(filePath: String)

  case class WordsMsgFileResult(wordsCount: Int)

  class WordsProcessor() extends Actor {

    private var processEnd = false
    private var wordsCount = 0
    private var fileCount = 0
    private var processedCount = 0
    private val resultPromise = Promise[Int]()

    def process(dirPath: String, fileExtensions: String*): concurrent.Future[Int] = {
      start()
      this ! WordsMsgProcess(dirPath, fileExtensions: _*)
      resultPromise.future
    }

    def act(): Unit = {
      loop {
        react {
          case msg: WordsMsgProcess =>
            startDirWorker(msg)
          case msg: WordsMsgFile =>
            fileCount += 1
            startFileWorker(msg)
          case WordsMsgFileResult(count) =>
            processedCount += 1
            wordsCount += count
            checkEnd()
          case _: WordsMsgProcessEnd =>
            processEnd = true
            checkEnd()
          case msg =>
            throw new IllegalStateException("Unknown message: " + msg)
        }
      }
    }

    def checkEnd(): Unit = {
      if (processedCount >= fileCount && processEnd) {
        resultPromise.success(wordsCount)
        exit()
      }
    }

    def startDirWorker(msg: WordsMsgProcess): Unit = {
      val dirWorker = new WordsDirWorker()
      dirWorker.start()
      dirWorker ! msg
    }

    def startFileWorker(msg: WordsMsgFile): Unit = {
      val fileWorker = new WordsFileWorker()
      fileWorker.start()
      fileWorker ! msg
    }
  }

  class WordsDirWorker() extends Actor {

    private var fileExtensions: scala.Seq[String] = null
    private var processor: OutputChannel[Any] = null
    private var dirCount = 1 // allow process the root directory

    def act(): Unit = {
      loop {
        react {
          case WordsMsgProcess(dirPath, fileExtensions @ _*) =>
            this.fileExtensions = fileExtensions
            this.processor = sender
            this ! WordsMsgDir(dirPath)
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
        else if (fileExtensions.isEmpty || fileExtensions.contains(Utils.getFileExt(file))) {
          // notify processor about new file
          processor ! WordsMsgFile(file.getPath)
        }
      }
    }
  }

  class WordsFileWorker() extends Actor {

    def act(): Unit = {
      react {
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

      var wordsCount = 0
      for (line <- Source.fromFile(file).getLines()) {
        WordsProgram.wordRegex.findAllIn(line).foreach(_ => wordsCount += 1)
      }

      reply(WordsMsgFileResult(wordsCount))
    }
  }
}
