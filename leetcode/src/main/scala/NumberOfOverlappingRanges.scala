import scala.annotation.tailrec

object NumberOfOverlappingRanges {

  /**
   * @param start The inclusive start of the range (starting at)
   * @param end The exclusive end of the range (up till)
   */
  case class IntRange(
    start: Int,
    end: Int
  )

  /**
   * For a set of input ranges, determine the maximum overlap.
   * Generate all start/end events. Sort them, with meeting endings coming before
   * meeting starts for any given time point, because the end is non-inclusive.
   * For starts, increment the number of rooms needed, for ends, decrement it.
   * Keep the max number of rooms founds.
   * @param input
   * @return
   */
  def solution(input: Vector[IntRange]): Int = {
    // Convert input into list of (time, eventFlag) where eventFlag is 0 if start or 1 if end
    val eventsSorted = input.flatMap(r => Seq((r.start, 1), (r.end, 0))).sorted
    var max = 0
    var current = 0
    for {
      (at, flag) <- eventsSorted
    } {
      if(flag == 1) {
        current = current + 1
      } else {
        current = current - 1
      }
      max = max.max(current)
    }
    max
  }

  case class TestCase(
    input: Vector[IntRange],
    expected: Int
  )

  def runTests(testCases: Vector[TestCase], f: Vector[IntRange] => Int) = {
    for {
      tc <- testCases
    } {
      val actual = f(tc.input)
      if(actual != tc.expected) {
        println(s"Failed, expected ${tc.expected}, got ${actual}")
      } else {
        println(s"Success, got $actual")
      }
    }
  }

  val testCases = Vector(
    TestCase(Vector.empty, 0),
    TestCase(Vector(IntRange(0, 1000)), 1),
    TestCase(Vector(IntRange(1000, 1000)), 0),
    TestCase(Vector(IntRange(0, 1000), IntRange(1000, 2000)), 1),
    TestCase(Vector(IntRange(0, 1000), IntRange(2000,3000)), 1),
    TestCase(Vector(IntRange(0, 1000), IntRange(0, 1000)), 2),
    TestCase(Vector(IntRange(0, 1000), IntRange(200, 800)), 2),
    TestCase(Vector(IntRange(0, 1000), IntRange(500, 1500)), 2),
    TestCase(Vector(IntRange(1000, 2000), IntRange(500, 1500), IntRange(1900, 2900)), 2),
    TestCase(Vector(IntRange(1000, 2000), IntRange(1000, 2000),IntRange(1000, 2000), IntRange(1500, 2500), IntRange(1500, 2500), IntRange(1500, 2500), IntRange(2000, 3000), IntRange(2000, 3000), IntRange(2000, 3000)), 6)
    )

  def main(args: Array[String]): Unit = {
    runTests(testCases, solution)
  }

}
