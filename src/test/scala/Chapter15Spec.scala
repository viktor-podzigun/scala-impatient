import Chapter15._
import Chapter15Task4.scalaSum
import Chapter15Task5.scalaFileToString
import TestUtils.{ClassPath, runApp, runCmd}
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

  "TailRecursion" should "provide non-overridden, tail-recursive method" in {
    //given
    val tr = new TailRecursion

    //when
    val result: Int = tr.sum(List(1, 2, 3, 4))

    //then
    result shouldBe 10
  }

  "allDifferent" should "be generated for all primitive types" in {
    //when
    val (exit, out, err) = runCmd("javap", "-classpath", ClassPath, "Chapter15$")

    //then
    exit shouldBe 0
    err shouldBe ""
    out should include ("public <T extends java/lang/Object> boolean allDifferent(T, T, T);")
    out should include ("public boolean allDifferent$mZc$sp(boolean, boolean, boolean);")
    out should include ("public boolean allDifferent$mBc$sp(byte, byte, byte);")
    out should include ("public boolean allDifferent$mCc$sp(char, char, char);")
    out should include ("public boolean allDifferent$mDc$sp(double, double, double);")
    out should include ("public boolean allDifferent$mFc$sp(float, float, float);")
    out should include ("public boolean allDifferent$mIc$sp(int, int, int);")
    out should include ("public boolean allDifferent$mJc$sp(long, long, long);")
    out should include ("public boolean allDifferent$mSc$sp(short, short, short);")
    out should include ("public boolean allDifferent$mVc$sp(scala.runtime.BoxedUnit, scala.runtime.BoxedUnit, scala.runtime.BoxedUnit);")
  }

  "factorial" should "throw an exception when assertions are enabled" in {
    //when & then
    a [AssertionError] should be thrownBy {
      factorial(-1)
    }
  }
}
