object FileSystem {
  /*
Design a data structure that simulates an in-memory file system.

Implement the FileSystem class:

FileSystem() Initializes the object of the system.

List<String> ls(String path)
  If path is a file path, returns a list that only contains this file's name.
  If path is a directory path, returns the list of file and directory names in this directory.
  The answer should in lexicographic order.

void mkdir(String path)
  Makes a new directory according to the given path. The given directory path does not exist.
  If the middle directories in the path do not exist, you should create them as well.

void addContentToFile(String filePath, String content)
  If filePath does not exist, creates that file containing given content.
  If filePath already exists, appends the given content to original content.

String readContentFromFile(String filePath)
  Returns the content in the file at filePath.

 */
  class FileSystem() {
    import collection.mutable

    private enum FSEntry {
      case Directory(contents: mutable.TreeMap[String, FSEntry] = mutable.TreeMap.empty)
      case File(segments: mutable.ListBuffer[String] = mutable.ListBuffer.empty)
    }

    import FSEntry._

    private val root = Directory()

    private type NotFoundCallback = (Directory, String) => Option[FSEntry]

    private val noActionOnNotFound: NotFoundCallback = (_, _) => None

    private def createOnNotFound(item: FSEntry): NotFoundCallback =
      (dir, nextPathElem) => {
        dir.contents.addOne(nextPathElem, item)
        Some(item)
      }

    private def walkTo(path: String, onNotFound: NotFoundCallback = noActionOnNotFound): Option[FSEntry] = {
      if (path == "/")
        Some(root)
      else {
        val pathElems = path.split("/").tail // get rid of empty element before "/" root prefix
        pathElems.foldLeft[Option[FSEntry]](Some(root)) {
          case (None, elem) => // Will be encountered if a previous element was not found - propagate "None"
            None
          case (Some(dir: Directory), elem) =>
            dir.contents.get(elem) match
              case e@Some(value) => e
              case None => onNotFound(dir, elem)
          case (Some(f: File), elem) => // We encountered a file but there are more path elements
            sys.error("Attempted to descend past file")
        }
      }
    }

    def ls(path: String): List[String] = {
      walkTo(path) match {
        case Some(Directory(contents)) => contents.keys.toList
        case Some(File(_)) => List(path.split("/").last)
        case None => sys.error("Not Found")
      }
    }

    def mkdir(path: String): Unit = {
      walkTo(path, createOnNotFound(Directory()))
    }

    def addContentToFile(filePath: String, content: String): Unit = {
      // assumes that directories were created before any file children
      walkTo(filePath, createOnNotFound(File())) match {
        case Some(File(segments)) => segments.addOne(content)
        case Some(d:Directory) => sys.error("Attempt to create file where directory exists")
        case None => sys.error("Not Found")
      }
    }

    def readContentFromFile(filePath: String): String = {
      walkTo(filePath) match {
        case Some(File(segments)) => segments.mkString
        case Some(d:Directory) => sys.error("Attempt to read directory as file")
        case None => sys.error("Not Found")
      }
    }

  }


}
