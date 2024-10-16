import scala.annotation.tailrec

object BillionUsersDay {

  def main(args: Array[String]): Unit = {
    assert(getBillionUsersDay(Array(1.5f)) == 52)
    assert(getBillionUsersDay(Array(1.5f, 1.5f)) == 50)
    assert(getBillionUsersDay(Array(2.0f)) == 30)
    assert(getBillionUsersDay(Array.fill(12)(1.2f)) == 101)
    assert(getBillionUsersDay(Array.fill(1000)(1.00001f)) == 1379685)
  }

  enum Comparison:
    case Low
    case Equal
    case High

  import Comparison._

  @tailrec
  def binarySearch(l: Int, r: Int, evaluate: Int => Comparison): Int = {
    println(s"Searching $l - $r")
    val m = (l + r) / 2
    evaluate(m) match {
      case Equal =>
        println(s"found $m")
        m
      case Low => binarySearch(m + 1, r, evaluate)
      case High => binarySearch(l, m, evaluate)
    }
  }

  def getBillionUsersDay(arr: Array[Float]): Int = {
    val BILLION = 1e9

    /** Compute the estimated number of users of all services on some day in the future */
    // O(n) : n = number of services
    def usersOnDay(t:Int) =
      arr.map(g => Math.pow(g, t)).sum.toFloat

    /** find left and right bounds that contain the point where the user count crosses 1B */
    // O(log n) : n = days required
    var left = 1
    var right = 2
    while (usersOnDay(right) < BILLION) {
      left = left * 2
      right = right * 2
    }

    println(s"starting point: $left - $right")
    // O(m * log n) : m = number of services, n = number of days
    binarySearch(left, right, { day =>
      if(usersOnDay(day - 1) >= BILLION)
        High
      else if(usersOnDay(day) < BILLION)
        Low
      else
        Equal
    })
  }

}
