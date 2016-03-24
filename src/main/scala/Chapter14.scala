

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

  /**
   * Task 5:
   *
   * One can use lists to model trees that store values only in the leaves. For example, the list
   * `((3 8) 2 (5))` describes the tree
   * {{{
   *     *
   *    /|\
   *   * 2 *
   *  /\   |
   * 3 8   5
   * }}}
   * However, some of the list elements are numbers and others are lists. In Scala, you cannot
   * have heterogeneous lists, so you have to use a `List[Any]`. Write a `leafSum` function to
   * compute the sum of all elements in the leaves, using pattern matching to differentiate
   * between numbers and lists.
   */
  def leafSum(xs: List[Any]): Double = {
    xs.foldLeft(0.0) {(acc: Double, item) => item match {
        case n: Number => acc + n.doubleValue()
        case list: List[_] => acc + leafSum(list)
        case _ => acc
      }
    }
  }

  /**
   * Task 6:
   *
   * A better way of modeling such trees is with case classes. Let's start with binary trees.
   * {{{
   * sealed abstract class BinaryTree
   * case class Leaf(value: Int) extends BinaryTree
   * case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree
   * }}}
   * Write a function to compute the sum of all elements in the leaves.
   */
  object Task6 {

    sealed abstract class BinaryTree

    case class Leaf(value: Int) extends BinaryTree

    case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree

    def leafSum(bt: BinaryTree): Int = bt match {
      case leaf: Leaf => leaf.value
      case left Node right => leafSum(left) + leafSum(right)
    }
  }

  /**
   * Task 7:
   *
   * Extend the tree in the preceding exercise so that each node can have an arbitrary number
   * of children, and reimplement the `leafSum` function. The tree in exercise 5 should be as
   * {{{
   *  Node(Node(Leaf(3), Leaf(8)), Leaf(2), Node(Leaf(5)))
   * }}}
   */
  object Task7 {

    sealed abstract class BinaryTree

    case class Leaf(value: Int) extends BinaryTree

    case class Node(children: BinaryTree*) extends BinaryTree

    def leafSum(bt: BinaryTree): Int = bt match {
      case leaf: Leaf => leaf.value
      case node: Node => node.children.foldLeft(0)((acc, item) => acc + leafSum(item))
    }
  }

  /**
   * Task 8:
   *
   * Extend the tree in the preceding exercise so that each non-leaf node stores an operator
   * in addition to the child nodes. Then write a function `eval` that computes the value.
   * For example, the tree
   * {{{
   *     +
   *    /|\
   *   * 2 -
   *  /\   |
   * 3 8   5
   * }}}
   * has value `(3 x 8) + 2 + (-5) = 21`.
   */
  object Task8 {

    sealed abstract class BinaryTree

    case class Leaf(value: Int) extends BinaryTree

    case class Node(op: Op, children: BinaryTree*) extends BinaryTree

    class Op private(val identity: Int, op: (Int, Int) => Int) {
      def apply(a: Int, b: Int): Int = op(a, b)
    }

    object Op {
      val Plus = new Op(0, _ + _)
      val Minus = new Op(0, _ - _)
      val Product = new Op(1, _ * _)
    }

    def eval(bt: BinaryTree): Int = bt match {
      case leaf: Leaf => leaf.value
      case node: Node => node.children.foldLeft(node.op.identity) { (acc, item) =>
        node.op(acc, eval(item))
      }
    }
  }

  /**
   * Task 9:
   *
   * Write a function that computes the sum of the non-None values in a `List[Option[Int]]`.
   * Don't use a match statement.
   */
  def sumOfNonNoneValues(xs: List[Option[Int]]): Int = xs.foldLeft(0) { (acc, item) =>
    acc + item.getOrElse(0)
  }
}
