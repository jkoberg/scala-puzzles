package us.jkk.examples.wordwrap

import scala.annotation.tailrec

object WordWrap {

  @tailrec
  def wrapRecursive(width:Int, remainingWords:List[String], currentLine:String = "", accumulatedOutput: Seq[String] = Seq.empty): Seq[String] = {
    val remainingWidth = if(currentLine.isEmpty) width else width - currentLine.length - 1
    if(remainingWidth <= 0)
      wrapRecursive(width, remainingWords, "", accumulatedOutput :+ currentLine)
    else
      remainingWords match {
        case Nil =>
          if(currentLine.nonEmpty) accumulatedOutput :+ currentLine else accumulatedOutput
        case word :: tail if word.length <= remainingWidth =>
          val updatedLine = if(currentLine.isEmpty) word else s"$currentLine $word"
          wrapRecursive(width, tail, updatedLine, accumulatedOutput)
        case word :: tail if word.length <= width =>
          wrapRecursive(width, remainingWords, "", accumulatedOutput :+ currentLine)
        case word :: tail if word.length > width =>
          val wordsWithSplit = word.slice(0, remainingWidth) :: word.slice(remainingWidth, Int.MaxValue) :: tail
          wrapRecursive(width, wordsWithSplit, currentLine, accumulatedOutput)
      }
  }

  def wordWrap(width:Int, message:String) = {
    val words: List[String] = message.split(raw"\s+").toList
    val output = wrapRecursive(width, words)
    output.mkString("\n")
  }
}


object WrappingTests {
  import WordWrap._

  // No words that are longer than a line
  def test1(): Unit = {
    val input = "one two three four five six seven"
    val limit1 = 10
    // ||||||||||
    // one two
    // three four
    // five six
    // seven
    val expected = Seq(
      "one two",
      "three four",
      "five six",
      "seven"
    ).mkString("\n")
    val actual = wordWrap(limit1, input)
    assert(actual == expected)
    println(actual)
  }


  // Word longer than a line that must be split
  def test2(): Unit = {
    val input = "one two three supercalifragilisticexpiealidocious four"
    val limit1 = 10
    // ||||||||||
    // one two
    // three supe
    // rcalifragi
    // listicexpi
    // ealidociou
    // s four
    val expected = Seq(
      "one two",
      "three supe",
      "rcalifragi",
      "listicexpi",
      "ealidociou",
      "s four"
    ).mkString("\n")
    val actual = wordWrap(limit1, input)
    assert(actual == expected)
    println(actual)
  }


  // Words of exactly the length of a line
  def test3(): Unit = {
    val input = "one two three supe rcalifragi listicexpi ealidociou s four"
    val limit1 = 10
    // ||||||||||
    // one two
    // three supe
    // rcalifragi
    // listicexpi
    // ealidociou
    // s four
    val expected = Seq(
      "one two",
      "three supe",
      "rcalifragi",
      "listicexpi",
      "ealidociou",
      "s four"
    ).mkString("\n")
    val actual = wordWrap(limit1, input)
    assert(actual == expected)
    println(actual)
  }

  val allTests = Seq(test1 _, test2 _, test3 _)
}


object Main extends App {
  import WrappingTests._

  for {
    test <- allTests
  } {
    test()
  }

  println("All tests ran")
}

