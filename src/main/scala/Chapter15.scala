import org.junit.Test

object Chapter15 {

  /**
   * Task 1:
   *
   * Write four `JUnit` test cases that use the `@Test` annotation with and without each of
   * its arguments. Run the tests with JUnit.
   */
  class Task1 {

    @Test
    def testCase1(): Unit = {
    }

    @Test(timeout = 500)
    def testCase2(): Unit = {
    }

    @Test(expected = classOf[RuntimeException])
    def testCase3(): Unit = {
      throw new RuntimeException()
    }

    @Test(timeout = 500, expected = classOf[RuntimeException])
    def testCase4(): Unit = {
      throw new RuntimeException()
    }
  }
}
