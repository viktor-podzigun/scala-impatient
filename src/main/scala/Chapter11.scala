import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Chapter11 {

  /**
   * Task 1:
   *
   * According to the precedence rules, how are `3 + 4 -> 5` and `3 -> 4 + 5` evaluated?
   *
   * Solution:
   *
   * Since `+` and `-` has the same precedence, then:
   * `3 + 4 -> 5` evaluated as `(3 + 4) -> 5` and
   * `3 -> 4 + 5` evaluated as `(3 -> 4) + 5`
   */

  /**
   * Task 2:
   *
   * The `BigInt` class has a `pow` method, not an operator.
   * Why didn't the Scala library designers choose `**` (as in Fortran) or `^` (as in Pascal)
   * for a power operator?
   *
   * Solution:
   *
   * First, the precedence is determined by the first character, the `**` operator would has
   * the same precedence as `*`, `/` operators, but `pow` operation should have higher precedence.
   * Second, the `^` operator is already exists for `BigInt` class, which is
   * "Bitwise exclusive-or of BigInts".
   */

  /**
   * Task 3:
   *
   * Implement the `Fraction` class with operations `+`, `-`, `*`, `/`. Normalize fractions,
   * for example turning `15/-6` into `-5/2`. Divide by the greatest common divisor, like this:
   * {{{
   *  class Fraction(n: Int, d: Int) {
   *    private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d)
   *    private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d)
   *    override def toString = num + "/" + den
   *    def sign(a: Int) = if (a > 0) 1 else if (a < 0) -1 else 0
   *    def gcd(a: Int, b: Int): Int = if (b == 0) abs(a) else gcd(b, a % b)
   *    ...
   *  }
   * }}}
   */
  class Fraction private(n: Int, d: Int) {

    val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d)

    val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d)

    private def sign(a: Int) =
      if (a > 0) 1
      else if (a < 0) -1
      else 0

    @tailrec
    private def gcd(a: Int, b: Int): Int =
      if (b == 0) Math.abs(a)
      else gcd(b, a % b)

    def +(other: Fraction): Fraction = sumOp(other, _ + _)

    def -(other: Fraction): Fraction = sumOp(other, _ - _)

    def *(other: Fraction): Fraction = Fraction(num * other.num, den * other.den)

    def /(other: Fraction): Fraction = Fraction(num * other.den, den * other.num)

    private def sumOp(other: Fraction, op: (Int, Int) => Int): Fraction = {
      Fraction(op(num * other.den, den * other.num), den * other.den)
    }

    override def equals(that: Any): Boolean = that match {
      case that: Fraction => num == that.num && den == that.den
      case _ => false
    }

    override def hashCode: Int = {
      val prime = 31
      var result = 1
      result = prime * result + num
      result = prime * result + den
      result
    }

    override def toString = num + "/" + den
  }

  object Fraction {
    def apply(n: Int, d: Int) = new Fraction(n, d)
  }

  /**
   * Task 4:
   *
   * Implement a class `Money` with fields for dollars and cents. Supply `+`, `-` operators
   * as well as comparison operators `==` and `<`. For example,
   * {{{
   *  Money(1, 75) + Money(0, 50) == Money(2, 25)
   *  }}}
   * should be true. Should you also supply `*` and `/` operators? Why or why not?
   *
   * Solution:
   *
   * Operators `*` and `/` for `Money` class doesn't make sense, since usually money multiplied
   * or divided by concrete number, not another amount of money.
   */
  class Money private(d: Int, c: Int) {

    if (d < 0) throw new IllegalArgumentException("d: " + d)
    if (c < 0) throw new IllegalArgumentException("c: " + c)

    val dollars: Int = if (c > 99) d + (c / 100) else d
    val cents: Int = if (c > 99) c % 100 else c

    def +(other: Money): Money = Money(dollars + other.dollars, cents + other.cents)

    def -(other: Money): Money = {
      val d: Int = dollars - other.dollars
      val c: Int = cents - other.cents
      if (c < 0) Money(d - 1, c + 100)
      else Money(d, c)
    }

    def ==(other: Money): Boolean = dollars == other.dollars && cents == other.cents

    def <(other: Money): Boolean = (dollars * 100) + cents < (other.dollars * 100) + other.cents

    override def toString = "%d.%02d".format(dollars, + cents)
  }

  object Money {
    def apply(d: Int, c: Int) = new Money(d, c)
  }

  /**
   * Task 5:
   *
   * Provide operators that construct an HTML table. For example,
   * {{{
   *  Table() | "Java" | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM, .NET"
   * }}}
   * should produce
   * {{{
   *  <table><tr><td>Java</td><td>Scala</td</tr><tr><td>Gosling...
   * }}}
   */
  class Table private {

    private val table = new ListBuffer[ListBuffer[String]]

    def |(cell: String): Table = {
      if (table.isEmpty) {
        table += new ListBuffer[String]
      }

      table.last += cell
      this
    }

    def ||(cell: String): Table = {
      table += new ListBuffer[String]
      table.last += cell
      this
    }

    def toHtml: String = {
      val sb = new StringBuilder("<table>")
      for (row <- table) {
        sb ++= "<tr>"
        for (cell <- row) {
          sb ++= "<td>"
          sb ++= cell
          sb ++= "</td>"
        }

        sb ++= "</tr>"
      }

      sb ++= "</table>"
      sb.toString()
    }

    override def toString = table.mkString("\n")
  }

  object Table {
    def apply() = new Table
  }

  /**
   * Task 6:
   *
   * Provide a class `ASCIIArt` whose objects contain figures such as
   * {{{
   *   /\_/\
   *  ( ' ' )
   *  (  -  )
   *   | | |
   *  (__|__)
   * }}}
   * Supply operators for combining two `ASCIIArt` figures horizontally
   * {{{
   *   /\_/\     -----
   *  ( ' ' )  / Hello \
   *  (  -  ) <  Scala |
   *   | | |   \ Coder /
   *  (__|__)    -----
   * }}}
   * or vertically. Choose operators with appropriate precedence.
   */
  class ASCIIArt(val rows: List[String] = Nil) {

    def +(row: String): ASCIIArt = new ASCIIArt(rows :+ row)

    def |(other: ASCIIArt): ASCIIArt = {
      val padLen = rows.foldLeft(0) {(len, s) =>
        if (len > s.length) len
        else s.length
      }
      val result = new ArrayBuffer[String]
      result ++= rows

      val buf = new StringBuilder
      var i = 0
      for (row <- other.rows) {
        if (i >= result.length) {
          result += ""
        }

        buf.clear()
        buf ++= result(i)
        while (buf.length < padLen) {
          buf.append(' ')
        }

        buf ++= row
        result(i) = buf.toString()
        i += 1
      }

      new ASCIIArt(result.toList)
    }

    override def toString = rows.mkString("\n")
  }

  /**
   * Task 7:
   *
   * Implement a class `BitSequence` that stores a sequence of 64 bits packed in a `Long` value.
   * Supply `apply` and `update` operators to get and set an individual bit.
   */
  class BitSequence {

    private var bits: Long = 0

    def apply(i: Int): Int = get(mask(i))

    def update(i: Int, bit: Int): Unit = set(mask(i), bit)

    private def mask(i: Int): Long = 1L << i

    private def get(mask: Long): Int =
      if ((bits & mask) == mask) 1
      else 0

    private def set(mask: Long, bit: Int): Unit =
      if (bit == 0) bits &= ~mask
      else bits |= mask
  }

  /**
   * Task 8:
   *
   * Provide a class `Matrix` - you can choose whether you want to implement `2 x 2` matrices,
   * square matrices of any size, or `m x n` matrices. Supply operations `+` and `*`.
   * The latter should also work with scalars, for example `mat * 2`.
   * A single element should be accessible as `mat(row, col)`.
   */
  class Matrix private(private val rows: Int,
                       private val cols: Int,
                       private val data: IndexedSeq[Int]) {

    def +(other: Matrix): Matrix = {
      val thisSize = size
      if (thisSize != other.size) {
        throw new IllegalArgumentException("matrices are not of the same size" +
          ", expected: " + thisSize + ", actual: " + other.size)
      }

      Matrix.init(rows, cols)((row, col) => this(row, col) + other(row, col))
    }

    def *(value: Int): Matrix =
      Matrix.init(rows, cols)((row, col) => this(row, col) * value)

    def *(other: Matrix): Matrix = {
      val (m1, n1) = size
      val (m2, n2) = other.size
      if (n1 != m2) {
        throw new IllegalArgumentException("matrices are not multiplication compatible" +
          ", expected rows: " + n1 + ", actual: " + m2)
      }

      Matrix.init(m1, n2) { (row, col) => (0 until n1).foldLeft(0)((sum, i) =>
        sum + data(row * n1 + i) * other.data(i * n2 + col))
      }
    }

    def apply(row: Int, col: Int): Int = data(row * cols + col)

    def size: (Int, Int) = (rows, cols)

    override def toString = {
      val sb = new StringBuilder
      for (row <- 0 until rows) {
        if (row > 0) {
          sb ++= "\n"
        }

        sb ++= "["
        for (col <- 0 until cols) {
          if (col > 0) {
            sb ++= ", "
          }

          sb.append(data(row * cols + col))
        }
        sb ++= "]"
      }

      sb.toString()
    }
  }

  object Matrix {
    def apply(rows: Int, cols: Int)(data: Int*): Matrix = {
      if ((rows * cols) != data.length) {
        throw new IllegalArgumentException("(" + rows + " x " + cols +
          ") data length: " + data.length)
      }

      new Matrix(rows, cols, data.toIndexedSeq)
    }

    private def init(rows: Int, cols: Int)(op: (Int, Int) => Int): Matrix = {
      val data = new Array[Int](rows * cols)
      for (row <- 0 until rows) {
        for (col <- 0 until cols) {
          data(row * cols + col) = op(row, col)
        }
      }

      new Matrix(rows, cols, data)
    }
  }

  /**
   * Task 9:
   *
   * Define an `unapply` operation for the `RichFile` class that extracts the file path, name,
   * and extension. For example, the `file/home/cay/readme.txt` has path `/home/cay`,
   * name `readme`, and extension `txt`.
   */
  object RichFile {
    
    private val pathNameExtRegex = """file(.*)/(.+)\.(.*)""".r

    def unapply(file: String): Option[(String, String, String)] = file match {
      case pathNameExtRegex(path, name, extension) => Some((path, name, extension))
      case _ => None
    }
  }

  /**
   * Task 10:
   *
   * Define an `unapplySeq` operation for the `RichFile` class that extracts all path segments.
   * For example, for the `file/home/cay/readme.txt`, you should produce a sequence of
   * three segments: `home`, `cay`, and `readme.txt`.
   */
  object RichFile2 {

    private val pathSeqRegex = """([^/]+)/?""".r

    def unapplySeq(file: String): Option[Seq[String]] = {
      val seq = new ListBuffer[String]
      for (pathSeqRegex(p) <- pathSeqRegex.findAllIn(file)) {
        seq += p
      }

      if (seq.nonEmpty && seq.head == "file") Some(seq.tail)
      else None
    }
  }
}
