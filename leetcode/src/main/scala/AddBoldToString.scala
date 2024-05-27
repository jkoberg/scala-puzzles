import scala.annotation.tailrec
import scala.collection.mutable

object AddBoldToStringTrie {

  class TrieNode {
    var wordEnd: Boolean = false
    private val children = mutable.HashMap.empty[Char, TrieNode]

    @tailrec
    final def append(key: String, keyIndex: Int = 0): TrieNode = {
      if (keyIndex == key.length)
        this.wordEnd = true
        this
      else
        val char = key.charAt(keyIndex)
        val child = children.getOrElseUpdate(char, TrieNode())
        child.append(key, keyIndex + 1)
    }

    @tailrec
    final def hasPrefix(prefix: String, keyIndex: Int = 0): Option[TrieNode] = {
      if(keyIndex == prefix.length)
        Some(this)
      else
        children.get(prefix.charAt(keyIndex)) match
          case Some(child) => child.hasPrefix(prefix, keyIndex + 1)
          case None => None
    }

    /**
     * starting at the beginning of s, check each character by incrementally descending down
     * the Trie. If we come across a word end, carry the current position forward as the longest found word.
     * If, at any point, we don't find the next character, return the current longest position.
     * If we reach the end of the string, return the current longest position
     * forward as the longest word length.
     * @param s The character sequence to scan for words
     * @param index The position we're currently examining
     * @param acc The longest word found so far
     * @return The length of the longest word in the trie that is found at the start of s
     */
    @tailrec
    final def longestWordFrom(s: Seq[Char], index:Int = 0, acc: Int = 0): Int = {
      if(index == s.length)
        acc
      else
        children.get(s(index)) match
          case Some(child) =>
            if(child.wordEnd)
              longestWordFrom(s, index + 1, acc + 1)
            else
              longestWordFrom(s, index + 1, acc)
          case None =>
            acc
    }
  }

  case class Range(var start: Int, var end: Int)

  def mergeRange(ranges: mutable.ArrayBuffer[Range], newRange: Range): Unit = {
    if(ranges.isEmpty || newRange.start > ranges.last.end + 1)
      ranges.addOne(newRange)
    else
      assert(newRange.start >= ranges.last.start)
      ranges.last.end = ranges.last.end.max(newRange.end)
  }

//  def addBoldTag(s: String, words: Array[String]): String = {
//    // First build a Trie of all words.
//    // Complexity: time: O(N) number of total chars in all words
//    //             space: O(N) number of total chars in all words
//    val wordTrie = TrieNode()
//    words.foreach { w => wordTrie.append(w) }
//
//    var position = 0
//    val view = s.view
//    val foundRanges = mutable.ArrayBuffer.empty[Range]
//    while(position < s.length) {
//      val longestFound = wordTrie.longestWordFrom(view.slice(position, view.length))
//      if(longestFound > 0) {
//        mergeRange(foundRanges, Range(position, position + longestFound))
//      }
//      position = position + 1
//    }
//    
//
//    val isBold = Array.fill(s.length)(false)
//    for {
//      range <- foundRanges
//      idx <- range.start.until(range.end)
//    } {
//      isBold(idx) = true
//    }
//  }

}

object AddBoldToString {


  def mergeRanges(ranges: Seq[(Int, Int)]): Seq[(Int, Int)] = {
    @tailrec
    def mergeRangesRec(ranges: Seq[(Int, Int)], output: Seq[(Int, Int)]): Seq[(Int, Int)] = {
      (output.last, ranges.headOption) match {
        case ((s1, e1), Some((s2, e2))) if s2 >  e1 + 1 =>
          mergeRangesRec(ranges.tail, output :+ ranges.head)
        case ((s1, e1), Some((s2, e2))) =>
          mergeRangesRec(ranges.tail, output.init :+ (s1, e1.max(e2)))
        case (_, None) =>
          output
      }
    }

    if(ranges.length <= 1)
      ranges
    else
      mergeRangesRec(ranges.tail, Seq(ranges.head))

  }

  def addBoldTag(s: String, words: Array[String]): String = {
    val v = s.view
    val foundWords = for {
      position <- v.indices
      word <- words
      if v.slice(position, v.length).startsWith(word)
    } yield {
      (position, position + word.length)
    }
    val mergedRanges = mergeRanges(foundWords)
    val mergedBracketed = (0,0) +: mergedRanges :+ (v.length, v.length)
    val output = StringBuilder()

    for {
      case Seq((s1, e1), (s2, e2)) <- mergedBracketed.sliding(2)
    } {
      if(e1 - s1 > 0) {
        output.append("<b>")
        output.append(v.slice(s1, e1))
        output.append("</b>")
      }
      output.append(v.slice(e1, s2))
    }
    output.toString()
  }



}
