import java.io._
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import scala.collection.mutable.ArrayBuffer
import scala.io.{BufferedSource, Source}
import scala.language.implicitConversions

object Chapter09 {

  /**
   * Task 1:
   *
   * Write a Scala code snippet that reverses the lines in a file
   * (making the last line the first one, and so on).
   */
  def reverseLines(file: File): Unit = {
    val source: BufferedSource = Source.fromFile(file)
    val lines = try {
      source.getLines().toBuffer.reverse
    }
    finally {
      source.close()
    }

    val writer = new PrintWriter(file)
    try {
      lines.foreach(writer.println)
    }
    finally {
      writer.close()
    }
  }

  /**
   * Task 2:
   *
   * Write a Scala program that reads a file with tabs, replaces each tab with spaces
   * so that tab stops are at n-column boundaries, and writes the result to the same file.
   */
  def replaceTabs(file: File, charsPerColumn: Int = 4): Unit = {
    var chars = 0
    val buff = new ArrayBuffer[Char]
    val source: BufferedSource = Source.fromFile(file)
    try for (c <- source) c match {
      case '\t' => for (_ <- 0 until (charsPerColumn - chars % charsPerColumn)) buff += ' '
        chars = 0
      case '\n' => buff += c
        chars = 0
      case _ => buff += c
        chars += 1
    }
    finally {
      source.close()
    }

    val writer = new PrintWriter(file)
    try {
      buff.foreach(writer.print)
    }
    finally {
      writer.close()
    }
  }

  /**
   * Task 3:
   *
   * Write a Scala code snippet that reads a file and prints all words with more than
   * 12 characters to the console. Extra credit if you can do this in a single line.
   */
  def printLongWords(file: String): Unit = {
    val maxWordLength: Int = 12
    Source.fromFile(file).mkString.split("\\s+").filter(_.length > maxWordLength).foreach(println)
  }

  /**
   * Task 4:
   *
   * Write a Scala program that reads a text file containing only floating-point numbers.
   * Print the sum, average, maximum, and minimum of the numbers in the file.
   */
  def printNumbersStat(file: String): Unit = {
    var count: Int = 0
    var sum: Double = 0.0
    var min: Double = Double.MaxValue
    var max: Double = Double.MinValue
    for (token <- Source.fromFile(file).mkString.split("\\s+"); num = token.toDouble) {
      count += 1
      sum += num
      if (num < min) min = num
      if (num > max) max = num
    }

    printf("sum:     %.3f\n", sum)
    printf("average: %.3f\n", sum / count)
    printf("minimum: %.3f\n", min)
    printf("maximum: %.3f\n", max)
  }

  /**
   * Task 5:
   *
   * Write a Scala program that writes the powers of 2 and their reciprocals to a file,
   * with the exponent ranging from 0 to 20. Line up the columns:
   * {{{
   *      1     1
   *      2     0.5
   *      4     0.25
   *      ...   ...
   * }}}
   */
  def printPowersOf2(file: File): Unit = {
    val writer = new PrintWriter(file)
    try {
      for (i <- 0 to 20) {
        writer.println("%8.0f  %f".format(math.pow(2.0, i), math.pow(2.0, -i)))
      }
    }
    finally {
      writer.close()
    }
  }

  /**
   * Task 6:
   *
   * Make a regular expression searching for quoted strings "like this, maybe with \" or \\"
   * in a Java or C++ program. Write a Scala program that prints out all such strings
   * in a source file.
   */
  def printQuotedStrings(file: String): Unit = {
    // got from here:
    // http://stackoverflow.com/questions/2498635/java-regex-for-matching-quoted-string-with-escaped-quotes
    val pattern = "\"(([^\\\\\"]+|\\\\([btnfr\"'\\\\]|[0-3]?[0-7]{1,2}|u[0-9a-fA-F]{4}))*)\"".r

    for (pattern(s, _, _) <- pattern.findAllIn(Source.fromFile(file).mkString)) {
      println(s)
    }
  }

  /**
   * Task 7:
   *
   * Write a Scala program that reads a text file and prints all tokens in the file
   * that are not floating-point numbers. Use a regular expression.
   */
  def printNonNumberTokens(file: String): Unit = {
    val pattern = "(?![\\d]+(\\.[\\d]+)?)\\w+".r

    for (s <- pattern.findAllIn(Source.fromFile(file).mkString)) {
      println(s)
    }
  }

  /**
   * Task 8:
   *
   * Write a Scala program that prints the `src` attributes of all `img` tags of a web page.
   * Use regular expressions and groups.
   */
  def printSrcOfImageTags(file: String): Unit = {
    val pattern = "(?i)<img\\s+(.*?\\s+)?src\\s*=\\s*\"([^\"]+)\"".r

    for (pattern(_, s) <- pattern.findAllIn(Source.fromFile(file).mkString)) {
      println(s)
    }
  }

  /**
   * Task 9:
   *
   * Write a Scala program that counts how many files with `.class` extension
   * are in a given directory and its subdirectories.
   */
  def countClassFiles(dir: String): Int = {
    var count = 0
    Files.walkFileTree(new File(dir).toPath, (f: Path) => {
      if (f.toString.endsWith(".class")) count += 1
    })

    count
  }

  implicit def makeFileVisitor(f: (Path) => Unit): FileVisitor[Path] = new SimpleFileVisitor[Path] {
    override def visitFile(p: Path, attrs: BasicFileAttributes) = {
      f(p)
      FileVisitResult.CONTINUE }
  }

  /**
   * Task 10:
   *
   * Expand the example with the serializable `Person` class that stores a collection of friends.
   * Construct a few `Person` objects, make some of them friends of another, and then
   * save an `Array[Person]` to a file. Read the array back in and verify that the friend
   * relations are intact.
   */
  class Person(val name: String) extends Serializable with Iterable[Person] {

    private val friends = new ArrayBuffer[Person]

    def addFriend(friend: Person): Person = {
      friends += friend
      this
    }

    override def iterator: Iterator[Person] = friends.toIterator
  }

  def savePersons(file: File, persons: Array[Person]): Unit = {
    val out = new ObjectOutputStream(new FileOutputStream(file))
    try {
      out.writeObject(persons)
    } finally {
      out.close()
    }
  }

  def readPersons(file: File): Array[Person] = {
    val in = new ObjectInputStream(new FileInputStream(file))
    try {
      in.readObject().asInstanceOf[Array[Person]]
    } finally {
      in.close()
    }
  }
}

object PrintLongWordsApp extends FileApp(Chapter09.printLongWords)

object PrintNumbersStatApp extends FileApp(Chapter09.printNumbersStat)

object PrintQuotedStringsApp extends FileApp(Chapter09.printQuotedStrings)

object PrintNonNumberTokensApp extends FileApp(Chapter09.printNonNumberTokens)

object PrintSrcOfImageTagsApp extends FileApp(Chapter09.printSrcOfImageTags)

sealed class FileApp(process: String => Unit) extends App {
  if (args.length < 1) {
    sys.error("Expect file name as first argument")
    System.exit(1)
  }

  process(args(0))
}
