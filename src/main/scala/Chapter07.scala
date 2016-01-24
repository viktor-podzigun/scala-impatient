
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
