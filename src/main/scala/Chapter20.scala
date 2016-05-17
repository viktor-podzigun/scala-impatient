import scala.actors.Actor

object Chapter20 {

  /**
   * Task 1:
   *
   * Write a program that generates an array of `n` random numbers (where `n` is a large value,
   * such as 1,000,000), and then computes the average of those numbers by distributing the work
   * over multiple actors, each of which computes the sum of the values, sending the result to
   * an actor that combines the results.
   *
   * If you run your program on a dual-core or quad-core processor, what is the speedup over
   * a single-threaded solution?
   */
  object RandCalc {

    val MaxWorkers = 10
    val NumPerWorker = 50000

    def calcAverageFor(count: Int): Double = {
      val proc = new RandProcessor()
      proc.start()

      val futureResult = proc !! RandMsgProcess(count)
      futureResult() match {
        case RandMsgResult(average) => average
      }
    }

    def calcAverage(range: Range): Double = {
      range.sum / range.size
    }
  }
  
  case class RandMsgProcess(n: Int)
  case class RandMsgCalc(range: Range)
  case class RandMsgResult(average: Double)
  
  class RandProcessor() extends Actor {

    private var count = 0
    private var processedCount = 0
    private var currAverage = 0.0

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

    private def startWorkers(n: Int): Unit = {
      this.count = n

      var workerCount = count / RandCalc.NumPerWorker
      if (workerCount == 0) workerCount = 1
      else if (workerCount > RandCalc.MaxWorkers) workerCount = RandCalc.MaxWorkers

      for (range <- getRanges(count, workerCount)) {
        val worker = new RandWorker()
        worker.start()
        worker ! RandMsgCalc(range)
      }
    }

    private def procResult(average: Double): Unit = {
      processedCount += 1
      currAverage += average
      
      if (processedCount >= count) {
        reply(RandMsgResult(currAverage / processedCount))
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
}
