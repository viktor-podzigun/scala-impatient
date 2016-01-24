
/**
 * Task 1:
 *
 * <p>Write an example program to demonstrate that
 * <blockquote><code>
 *   package com.horstmann.impatient
 * </code></blockquote>
 * is not the same as
 * <blockquote><code>
 *   package com <br/>
 *   package horstmann <br/>
 *   package impatient <br/>
 * </code></blockquote>
 *
 * @see Chapter0701a.scala  for solution part A
 * @see Chapter0701b.scala  for solution part B
 */
package com {

  object FromCom {
    val value = 1
  }

  package horstmann {

    object FromHorstmann {
      val value = 2
    }

    package impatient {

      object FromImpatient {
        val value = 3
      }
    }
  }
}

/**
 * Task 2:
 *
 * <p>Write a puzzler that baffles your Scala friends, using a package <code>com</code>
 * that isn’t at the top level.
 */
package puzzler {

  package com {
    object FromCom {
      val value = 21
    }
  }
}

/**
 * Task 3:
 *
 * <p>Write a package random with functions
 * nextInt(): Int,
 * nextDouble(): Double,
 * and setSeed(seed: Int): Unit.
 *
 * <p>To generate random numbers, use the linear congruential generator
 * next = previous × a + b mod 2n,
 * where a = 1664525, b = 1013904223, and n = 32.
 */
package object random {

  private val addition: Int = (1013904223 % (1L << 32)).toInt
  private var seed : Int = 0

  def nextInt(): Int = {
    seed = (seed * 1664525) + addition

    if (seed < 0) ~seed
    else seed
  }

  def nextDouble(): Double = {
    nextInt() / (Int.MaxValue + 1.0)
  }

  def setSeed(seed: Int): Unit = this.seed = seed
}

/**
 * Task 4:
 *
 * <p>Why do you think the Scala language designers provided the package object syntax instead
 * of simply letting you add functions and variables to a package?
 *
 * <p>Solution: <br/>
 * They decided to make it explicit by adding just one word "object" to package declaration,
 * in my opinion, for a couple of reasons:
 * <ul>
 *   <li>since its possible to have package declarations in different files, it would be hard
 *   to maintain functions and variable in different places for the same package</li>
 *   <li>because variables in package object are global (singletons) they didn't want to make it
 *   available by default</li>
 * </ul>
 */

/**
 * Task 5:
 *
 * <p>What is the meaning of <code>private[com] def giveRaise(rate: Double)</code>?
 * Is it useful?
 */
package com {

  /**
   * <code>private[com]</code> makes definition package-private, meaning it is visible within
   * the same package and all sub-packages.
   */
  object VisibilityDef {
    private[com] def giveRaise(rate: Double): Double = rate * 0.5
  }

  object VisibilityUsage {
    println(VisibilityDef.giveRaise(1))
  }

  package horstmann {

    object VisibilityUsage {
      println(VisibilityDef.giveRaise(1))
    }
  }
}

/**
 * Task 6:
 *
 * <p>Write a program that copies all elements from a Java hash map into a Scala hash map.
 * Use imports to rename both classes.
 */
object Chapter0706 {

  /**
   * Task 7:
   *
   * <p>In the preceding exercise, move all imports into the innermost scope possible.
   */
  import java.util.{HashMap => JavaHashMap}

  import scala.collection.mutable.{HashMap => ScalaHashMap}

  def fromJavaHashMap(javaHashMap: JavaHashMap[String, Int]): ScalaHashMap[String, Int] = {
    import scala.collection.JavaConversions.iterableAsScalaIterable

    val result = new ScalaHashMap[String, Int]
    for (entry <- javaHashMap.entrySet()) {
      result(entry.getKey) = entry.getValue
    }

    result
  }
}

/**
 * Task 8:
 *
 * <p>What is the effect of
 * <blockquote><code>
 *   import java._ <br/>
 *   import javax._ <br/>
 * </code></blockquote>
 * Is this a good idea?
 */
object Chapter0708 {

  import java._

  /**
   * Since we imported everything from java package, we can use sub-packages.
   */
  def doSomething(evt: util.List) {
  }
}
