import java.io.{ByteArrayOutputStream, File, PrintWriter}
import scala.collection.JavaConversions.seqAsJavaList
import scala.collection.mutable
import scala.io.Source

/**
 * Contains utility functions and constants used in tests.
 */
object TestUtils {

  val ClassPath = "./target/scala-2.11/classes"

  // should be the same as in build script
  private val ScalaVersion = "2.11.7"

  def runApp(mainObj: String, args: String*): (Int, String, String) = {
    runAppWithInput("", mainObj, args: _*)
  }

  def runAppWithInput(input: String, mainObj: String, args: String*): (Int, String, String) = {
    val jars: String = System.getProperty("user.home") + "/.ivy2/cache"
    val cmd = mutable.Buffer("java",
      "-cp", jars + "/org.scala-lang/scala-library/jars/scala-library-" + ScalaVersion + ".jar" +
        File.pathSeparator + jars +
        "/org.scala-lang.modules/scala-xml_2.11/bundles/scala-xml_2.11-1.0.2.jar" +
        File.pathSeparator + jars +
        "/org.scoverage/scalac-scoverage-runtime_2.11/jars/scalac-scoverage-runtime_2.11-1.1.1.jar" +
        File.pathSeparator + jars +
        "/junit/junit/jars/junit-4.11.jar" +
        File.pathSeparator + jars +
        "/org.hamcrest/hamcrest-all/jars/hamcrest-all-1.3.jar" +
        File.pathSeparator + ClassPath)
    cmd += mainObj
    cmd ++= args

    runCmdWithInput(input, cmd: _*)
  }

  def runCmd(cmd: String*): (Int, String, String) = {
    runCmdWithInput("", cmd: _*)
  }

  def runCmdWithInput(input: String, cmd: String*): (Int, String, String) = {
    val process = new ProcessBuilder(cmd).start()
    if (!input.isEmpty) {
      val writer = new PrintWriter(process.getOutputStream)
      writer.println(input)
      writer.flush()
    }

    val out = Source.fromInputStream(process.getInputStream).mkString
    val err = Source.fromInputStream(process.getErrorStream).mkString
    process.waitFor()

    (process.exitValue(), out, err)
  }

  def printToTmpFile(fileName: String, text: String): File = {
    val file = File.createTempFile(fileName, "txt")
    file.deleteOnExit()

    val writer = new PrintWriter(file)
    try {
      writer.print(text)
      file
    }
    finally {
      writer.close()
    }
  }
  
  def withOutput(block: => Unit): String = {
    val out = new ByteArrayOutputStream()
    Console.withOut(out)(block)
    out.toString
  }
}
