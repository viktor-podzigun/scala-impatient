

object Chapter14 {

  /**
   * Task 1:
   *
   * Your Java Development Kit distribution has the source code for much of the JDK in the
   * `src.zip` file. Unzip and search for case labels (regular expression `case [^:]+:`).
   * Then look for comments starting with `//` and containing `[Ff]alls? thr` to catch comments
   * such as `// Falls through` or `// just fall thru`.
   * Assuming the JDK programmers follow the Java code convention, which requires such a comment,
   * what percentage of cases falls through?
   *
   * Solution:
   * {{{
   *  mkdir /tmp/java-test/src
   *  unzip /usr/lib/jvm/java-7-oracle/src.zip -d /tmp/java-test/src
   *  cd /tmp/java-test
   *  grep --include=*.java -r -E 'case [^:]+:' src > 1.txt
   *  grep --include=*.java -r -E '//.*[Ff]alls? thr' src > 2.txt
   * }}}
   * As a result on my machine there is around 9500 lines in the first file and
   * 100 lines in the second file. Then:
   * {{{
   *  (100 * 100%) / 9500 = 1%
   * }}}
   */

  /**
   * Task 2:
   *
   * Using pattern matching, write a function `swap` that receives a pair of integers and
   * returns the pair with the components swapped.
   */
  def swap(pair: (Int, Int)): (Int, Int) = pair match {
    case (one, two) => (two, one)
  }

  /**
   * Task 3:
   *
   * Using pattern matching, write a function `swap` that swaps the first two elements of
   * an array provided its length is at least two.
   */
  def swap2(arr: Array[Int]): Array[Int] = arr match {
    case Array(first, second, _*) =>
      arr(0) = second
      arr(1) = first
      arr
    case _ => arr
  }

  /**
   * Task 4:
   *
   * Add a case class `Multiple` that is a subclass of the `Item` class. For example,
   * `Multiple(10, Article("Blackwell Toaster", 29.95))` describes ten toasters. Of course,
   * you should be able to handle any items, such as bundles or multiples, in the second argument.
   * Extend the `price` function to handle this new case.
   */
  sealed abstract class Item
  case class Article(description: String, price: Double) extends Item
  case class Bundle(description: String, discount: Double, items: Item*) extends Item
  case class Multiple(count: Int, item: Item) extends Item

  def price(it: Item): Double = it match {
    case Article(_, p) => p
    case Bundle(_, disc, its @ _*) => its.map(price).sum - disc
    case Multiple(count, item) => count * price(item)
  }
}
