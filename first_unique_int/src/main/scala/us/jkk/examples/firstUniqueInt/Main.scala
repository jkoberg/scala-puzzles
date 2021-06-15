package us.jkk.examples.firstUniqueInt

import scala.collection.mutable

trait CharStream {
  def hasNext: Boolean
  def getNext: Char
}

case class StringStream(buffer:String) extends CharStream {
  var index = 0

  def hasNext: Boolean = index < buffer.length

  def getNext: Char =
    if(hasNext) {
      val next = buffer.charAt(index)
      index += 1
      next
    } else {
      sys.error("End of stream reached")
    }
}


object FirstUniqueTests {
  case class TestCase(expectation: Option[Char], input:String)

  val suite = Seq(
    TestCase(None, ""),
    TestCase(None, "ABCABC"),
    TestCase(Some('A'), "ABCDEFG"),
    TestCase(Some('C'), "ABCDEFGAB")
  )
}

object Main extends App {

  def findFirstUnique(s:CharStream): Option[Char] = {
    var encountered: mutable.Set[Char] = mutable.Set.empty
    var currentFirstUnique: Option[Char] = None
    while(s.hasNext) {
      val c = s.getNext
      if(currentFirstUnique.contains(c)) {
        currentFirstUnique = None
      } else {
        
      }
      if(encountered.contains(c)) {
        
      } else {
        
      }
    }
    currentFirstUnique
  }


  for((test, idx) <- FirstUniqueTests.suite.zipWithIndex) {
    val actual = findFirstUnique(StringStream(test.input))
    if(actual == test.expectation) {
      println(s"#${idx}: SUCCESS")
    } else {
      println(s"#${idx}: FAILURE, was ${actual}, should be ${test.expectation}")
    }
  }
}
