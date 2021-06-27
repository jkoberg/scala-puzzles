import java.io.File

object JavaWindowsFileSystem extends Filesystem:

  case class WindowsPathInfo(f:File) extends PathInfo:
    
    override def name: String = f.getPath
    
    override def traverse: Boolean =
      val relativeName = f.getName
      (f.isDirectory &&
        relativeName != "." &&
        relativeName != ".." &&
        !relativeName.startsWith("$")
        )
  
  
  
  override def getPathInfo(pathName: String): PathInfo =
    WindowsPathInfo(new File(pathName))

  
  override def listDir(dirName: String): Seq[PathInfo] =
    for
      children <- Option(new File(dirName).listFiles()).toSeq
      child <- children.sortInPlace().toSeq
    yield
      WindowsPathInfo(child)

