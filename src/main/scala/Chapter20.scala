import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.actors.Actor
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
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

    def invert(srcFile: File, dstFile: File) {
      val bufferedImage = ImageIO.read(srcFile)
      invertImage(bufferedImage)
      ImageIO.write(bufferedImage, Utils.getFileExt(srcFile), dstFile)
    }

    private def invertImage(src: BufferedImage): Unit = {
      for (x <- 0 until src.getWidth) {
        val colors = new Array[Int](src.getHeight)
        for (y <- 0 until src.getHeight) {
          colors(y) = src.getRGB(x, y)
        }

        for (i <- colors.indices) {
          var col = new Color(colors(i), true)
          col = new Color(255 - col.getRed, 255 - col.getGreen, 255 - col.getBlue)
          colors(i) = col.getRGB
        }

        for (y <- colors.indices) {
          src.setRGB(x, y, colors(y))
        }
      }
    }
  }
}
