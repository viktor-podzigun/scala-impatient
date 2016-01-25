import java.io.{PrintWriter, File}

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
    val cmd = mutable.Buffer("java",
      "-cp", System.getProperty("user.home") + "/.m2/repository/org/scala-lang/scala-library/" +
        ScalaVersion + "/scala-library-" + ScalaVersion + ".jar" +
        File.pathSeparator + "./target/classes")
    cmd += mainObj
    cmd ++= args
    
    val process = new ProcessBuilder(cmd).start()
    if (!input.isEmpty) {
      val writer = new PrintWriter(process.getOutputStream)
      writer.println(input)
      writer.flush()
    }

    val out = Source.fromInputStream(process.getInputStream).getLines().mkString("\n")
    val err = Source.fromInputStream(process.getErrorStream).getLines().mkString("\n")
    process.waitFor()

    (process.exitValue(), out, err)
  }
}
