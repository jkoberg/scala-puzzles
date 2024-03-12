import scala.collection.mutable.ArrayBuffer


object ThreeSum {
  def threeSum(nums: Array[Int]): List[List[Int]] = {
    val output = scala.collection.mutable.HashSet.empty[List[Int]]

    util.Sorting.quickSort(nums)

    for {
      (ni, i) <- nums.zipWithIndex
      if i < nums.length -2
     } do {
      var j = i + 1
      var k = nums.length - 1
      while (j < k) {
        val nj = nums(j)
        val nk = nums(k)
        val sum = ni + nj + nk
        println(s"$ni + $nj + $nk == $sum")
        sum match {
          case s if s == 0 =>
            output.addOne(List(ni, nj, nk))
            j = j + 1
            k = k - 1
          case s if s > 0 =>
            k = k - 1
          case s if s < 0 =>
            j = j + 1
        }
      }
    }
    output.toList
  }


  case class TestCase(input: Array[Int], expected: Seq[Seq[Int]])

  def main(args: Array[String]): Unit = {
    val cases = Seq(
      TestCase(Array(-1,0,1,2,-1,-4), Seq(Seq(-1,-1,2),Seq(-1,0,1))),
      TestCase(Array(0, 1, 1), Seq()),
      TestCase(Array(0, 0, 0), Seq(Seq(0,0,0)))
    )

    for {
      c <- cases
    } do {
      val actual = threeSum(c.input)
      if(actual == c.expected) {
        println("Pass!")
      } else {
        println(s"Fail on ${c.input}\nGot: ${actual}\nExpected: ${c.expected}")
      }
    }


  }


}
