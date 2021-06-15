package us.jkk.examples.parser

import scala.annotation.tailrec

object Parser {
  val beginToks = List("begin", "for", "if")
  val endToks = List("end", "loop", "endif")
  val blockMap = beginToks.zip(endToks).toMap

  def tokenize(s: String): List[String] =
    s.split(raw"\s+")
      .filter(_.nonEmpty)
      .map(_.toLowerCase)
      .toList

  @tailrec
  def parse(tokens: List[String], stack: List[String] = List.empty): Either[String, String] =
    (tokens, stack) match {
      case (Nil, Nil) =>
        // End of program
        Right("Finished")

      case (Nil, innermostEnd :: stk) =>
        // End of program but unclosed block remains on stack.
        Left(s"Unclosed block, expected '$innermostEnd'")

      case (startTok :: rest, stk) if blockMap.contains(startTok) =>
        // "start" token  push the end token corresponding to
        // this start token onto the stack for future matching
        val newStack = blockMap(startTok) :: stk
        parse(rest, newStack)

      case (tok :: rest, innermostEnd :: stk) if tok == innermostEnd =>
        // Close innermost block - pop this block off the stack
        parse(rest, stk)

      case (tok :: rest, stk) if endToks.contains(tok) =>
        // end-block keyword without opening keyword
        Left(s"Unexpected keyword: $tok")

      case (tok :: rest, stk) =>
        // Consume non-keyword token
        parse(rest, stk)
    }

  def validate(text: String): Boolean = {
    parse(tokenize(text)) match {
      case Right(msg) =>
        println(msg)
        true
      case Left(reason) =>
        println(reason)
        false
    }
  }
}

