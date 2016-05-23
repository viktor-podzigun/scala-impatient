import java.io.{File, IOException}
import scala.collection.immutable.Seq
import scala.collection.mutable.ArrayBuffer

object Utils {

  abstract class FileApp(process: String => Unit) extends App {
    if (args.length < 1) {
      sys.error("Expect file name as first argument")
      System.exit(1)
    }

    process(args(0))
  }

  def getFileExt(file: File): String = {
    val name = file.getName
    name.substring(name.lastIndexOf('.') + 1)
  }

  def listAllFiles(dirPath: String, fileExtensions: String*): Seq[File] = {
    val dir = new File(dirPath)
    require(dir.isDirectory, s"Given path should represent directory: $dirPath")

    val buf = new ArrayBuffer[File]()
    def traverse(dir: File): Unit = {
      val files = dir.listFiles()
      if (files == null) {
        throw new IOException(s"Cannot read directory content: $dir")
      }

      for (file <- files) {
        if (file.isDirectory) traverse(file)
        else if (fileExtensions.isEmpty || fileExtensions.contains(getFileExt(file))) {
          buf += file
        }
      }
    }

    traverse(dir)
    buf.toIndexedSeq
  }
}
