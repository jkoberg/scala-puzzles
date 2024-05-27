import scala.annotation.tailrec

object ColorfulNumber {
  def digitSubSequences(s: String): Seq[Seq[Int]] = {
    for {
      startPos <- s.indices
      endPos <- startPos.until(s.length)
    } yield {
      s.substring(startPos, endPos).map(c => c.asDigit).sorted
    }
  }

  @tailrec
  def isColorfulRec(seqs: Seq[Seq[Int]], seen: Set[Seq[Int]] = Set.empty): Boolean = {
    seqs match {
      case Nil =>
        true
      case subsequence :: more =>
        if(seen.contains(subsequence)) 
          false
        else
          isColorfulRec(more, seen + subsequence)
    }
  }

  def isColorfulNumber(n: Int): Boolean = {
    val digits = n.toString
    isColorfulRec(digitSubSequences(n.toString))
  }
}
