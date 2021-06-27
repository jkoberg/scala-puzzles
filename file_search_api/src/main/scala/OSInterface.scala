trait PathInfo:
  def name: String
  def traverse: Boolean

trait Filesystem:
  def listDir(dirName: String): Seq[PathInfo]
  def getPathInfo(pathName: String): PathInfo
