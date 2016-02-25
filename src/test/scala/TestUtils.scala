import java.io.{File, PrintWriter}

import scala.collection.JavaConversions.bufferAsJavaList
import scala.collection.mutable
import scala.io.Source

/**
 * Contains utility functions and constants used in tests.
 */
object TestUtils {

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
        "/org.scoverage/scalac-scoverage-runtime_2.11/jars/scalac-scoverage-runtime_2.11-1.1.1.jar" +
        File.pathSeparator + "./target/scala-2.11/classes")
    cmd += mainObj
    cmd ++= args
    
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
}
