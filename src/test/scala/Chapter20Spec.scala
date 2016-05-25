import Chapter20._
import java.awt.Color
import javax.imageio.ImageIO
import org.scalatest.{FlatSpec, Matchers}

class Chapter20Spec extends FlatSpec with Matchers {

  "RandCalc" should "compute the average of random numbers" in {
    //given
    val n = 1000000

    //when
    val average: Double = RandCalc.calcAverageFor(n, useActors = true)

    //then
    average should be > 0.0
  }

  "ImageProgram" should "invert colors of the large image" in {
    //given
    val tmpFile = TestUtils.resourceToTmpFile("/scaladays.png")
    val sourceImage = ImageIO.read(tmpFile)

    //when
    ImageProgram.invert(tmpFile, tmpFile)

    //then
    val invertedImage = ImageIO.read(tmpFile)
    sourceImage.getWidth shouldBe invertedImage.getWidth
    sourceImage.getHeight shouldBe invertedImage.getHeight
    for (x <- 0 until sourceImage.getWidth) {
      for (y <- 0 until sourceImage.getHeight) {
        var col = new Color(sourceImage.getRGB(x, y), true)
        col = new Color(255 - col.getRed, 255 - col.getGreen, 255 - col.getBlue)

        invertedImage.getRGB(x, y) shouldBe col.getRGB
      }
    }
  }

  "WordsCountProgram" should "count matched words in all files and subdirectories" in {
    //given
    val dirPath = "src/main/"
    val fileExtensions = List("txt", "html", "xhtml")

    //when
    val result: Int = WordsCountProgram.calcMatchedWords(dirPath, fileExtensions: _*)

    //then
    result shouldBe 3
  }

  "WordsPrintProgram" should "print matched words in all files and subdirectories" in {
    //given
    val dirPath = "src/main/"
    val fileExtensions = List("txt", "html", "xhtml")

    //when
    val result: String = WordsPrintProgram.printMatchedWords(dirPath, fileExtensions: _*)

    //then
    result shouldBe """text
                      |text
                      |text
                      |""".stripMargin
  }
}
