package us.jkk.examples.parser

/**
 * Coding Exercise - Scala
 *
 * Your team has decided to offer customers a simple language to extend product
 * functionality in a safe way.
 *
 * This language will allow blocks and control structures of the following form:
 *
 * begin ... end
 * if ..... endif
 * for .... loop
 *
 * A valid program consists of zero or more commands or blocks.  A block may contain another
 * valid program. Every block that's opened must be closed.  Whitespace is not significant,
 * but serves only as a separator. The above keywords are not otherwise allowed in a program.
 *
 * Write code that takes a program string as input and returns a boolean indicating whether the
 * program represented by the string is a valid program. We have provided some programs as
 * test cases.
 */
object TestSuite:

  def validate(program: String): Boolean =
  // insert your code here
    false

  val testPrograms = Seq(
    "" -> true,
    "hello" -> true,
    "hello goodbye" -> true,
    "begin" -> false,
    "begin end" -> true,
    "hello begin" -> false,
    "hello end" -> false,
    "hello begin end" -> true,
    "begin foo bar quux end" -> true,
    "begin if for a b c loop d e f endif g h i end" -> true,
    "foo bar if quux end" -> false,
    "foo begin for end loop bar" -> false,
    "end begin" -> false
  )


  def main(args: Array[String]): Unit =
    for
      (program, expectation) <- testPrograms
    do
      validate(program) match
        case result if result == expectation => println("Test passed.")
        case _ => sys.error("Test failed.")
