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

  /**
   * Task 2:
   *
   * Make an example class that shows every possible position of an annotation.
   * Use `@deprecated` as your sample annotation.
   *
   * Solution:
   *
   * I've used `@unchecked` instead to avoid compilation warnings, since `@deprecated` now takes
   * two arguments.
   */
  @unchecked
  class Deprecated[@unchecked T] @unchecked() (@unchecked val field: Int) {

    @unchecked
    type Dep = Int @unchecked

    @unchecked
    var field2: Int @unchecked = 0

    @unchecked
    def unchecked(@deprecatedName('oldArg) newArg: Int): Int = {
      // annotation on an expression
      1 + 2: @unchecked
    }
  }

  /**
   * Task 3:
   *
   * Which annotations from the Scala library use one of the meta-annotations
   * {{{
   *  @param, @field, @getter, @setter, @beanGetter, or @beanSetter?
   * }}}
   *
   * Solution:
   *
   * - `@deprecated`
   * - `@deprecatedName`
   * - `@BeanProperty`
   */
}
