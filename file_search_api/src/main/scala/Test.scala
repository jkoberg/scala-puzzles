import Api.RegexRule


object Test extends App:
  given Filesystem = JavaWindowsFileSystem

  Api.ls(
    raw"c:\\",
    RegexRule(raw".*\.dll".r)
  )
