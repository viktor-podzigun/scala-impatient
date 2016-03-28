import Chapter15._
import TestUtils.runApp
import Chapter15Task4.scalaSum
import Chapter15Task5.scalaFileToString
import java.io.IOException
import org.scalatest.{FlatSpec, Matchers}

class Chapter15Spec extends FlatSpec with Matchers {

  "Task1" should "has four test cases that use the org.junit.Test annotation" in {
    //when
    val (exit, out, err) = runApp("org.junit.runner.JUnitCore", "Chapter15$Task1")

    //then
    exit shouldBe 0
    err shouldBe ""
    out should include ("OK (4 tests)")
  }

  "scalaSum" should "call Scala sum method with variable arguments from Java" in {
    //when & then
    scalaSum(1, 2, 3) shouldBe 6
    sum(1, 2, 3, 4) shouldBe 10
  }

  "scalaFileToString" should "call Scala fileToString method" in {
    //when & then
    scalaFileToString("/myfile.txt") shouldBe """
                                                |Simple text file with example words.
                                                |We will parse the file and count the words.
                                                |""".stripMargin
    a [RuntimeException] should be thrownBy {
      scalaFileToString("nonExisting.file")
    }
    a [IOException] should be thrownBy {
      fileToString("nonExisting.file")
    }
  }

  "Work" should "emulate work with two threads and one shared volatile boolean flag" in {
    //when
    val (exit, out, err) = runApp("Chapter15WorkApp")

    //then
    exit shouldBe 0
    err shouldBe ""
    out should include ("Work.done flag was set to true")
    out should include ("Work is done, exiting")
  }
}
