

object Utils {

  abstract class FileApp(process: String => Unit) extends App {
    if (args.length < 1) {
      sys.error("Expect file name as first argument")
      System.exit(1)
    }

    process(args(0))
  }
}
