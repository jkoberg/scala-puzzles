import scala.util.matching.Regex

object Api:

  trait Rule:
    def evaluate(p: PathInfo): Boolean


  case class AndRule(rules: Seq[Rule]) extends Rule:
    override def evaluate(p: PathInfo): Boolean =
      rules.forall(_.evaluate(p))


  case class OrRule(rules: Seq[Rule]) extends Rule:
    override def evaluate(p: PathInfo): Boolean =
      rules.exists(_.evaluate(p))


  case class RegexRule(pattern: Regex) extends Rule:
    override def evaluate(p: PathInfo): Boolean =
      pattern.matches(p.name)


  def ls(root: PathInfo, rule: Rule)(using fs: Filesystem): Unit =
    if(rule.evaluate(root))
      println(root.name)

    if(root.traverse)
        for
          child <- fs.listDir(root.name)
        do
          ls(child, rule)

  def ls(root: String, rule: Rule)(using fs: Filesystem): Unit =
    ls(fs.getPathInfo(root), rule)

