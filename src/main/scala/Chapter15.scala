import java.io.IOException
import org.junit.Test
import scala.annotation.varargs
import scala.io.Source

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

  /**
   * Task 4:
   *
   * Write a Scala method `sum` with variable integer arguments that returns the sum of its
   * arguments. Call it from Java.
   *
   * @see Chapter15Task4.java
   */
  @varargs
  def sum(args: Int*): Int = {
    args.sum
  }

  /**
   * Task 5:
   *
   * Write a Scala method that returns a string containing all lines of a file. Call it from Java.
   */
  @throws[IOException]
  def fileToString(file: String): String = {
    val inStream = getClass.getResourceAsStream(file)
    if (inStream == null) {
      throw new IOException("Resource is not found: " + file)
    }

    Source.fromInputStream(inStream).mkString
  }

  /**
   * Task 6:
   *
   * Write a Scala object with a `volatile` `Boolean` field. Have one thread sleep for some time,
   * then set the field to `true`, print a message, and exit. Another thread will keep checking
   * whether the field is `true`. If so, it prints a message and exits. If not, it sleeps for
   * a short time and tries again. What happens if the variable is not volatile?
   *
   * Solution:
   *
   * If the variable is not volatile, then threads may see the changes to this variable with some
   * delay, or may not see changes at all. Since non-volatile (normal) variables may be cached.
   */
  object Work {

    @volatile
    private var done = false

    def doWork(): Unit = {
      val threads = List(new Thread(new Runnable {

        override def run(): Unit = {
          Thread.sleep(200)

          Work.done = true
          println("Work.done flag was set to true")
        }
      }), new Thread(new Runnable {

        override def run(): Unit = {
          while (!Work.done) {
            println("Work is not done yet, waiting...")
            Thread.sleep(100)
          }

          println("Work is done, exiting")
        }
      }))

      threads.foreach(_.start())
      threads.foreach(_.join())
    }
  }
}

object Chapter15WorkApp extends App {

  Chapter15.Work.doWork()
}
