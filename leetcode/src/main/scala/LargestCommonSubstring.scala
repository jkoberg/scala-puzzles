

object LargestCommonSubstring {

  /**
   * Generate all substrings, from largest to smallest.
   *
   * If largest is generated first, we can terminate early when
   * we find a match, since it will be the maximal length match.
   * @param s
   * @return
   */
  def all_substrings(s: String): IterableOnce[String] = {
    if (s.isEmpty)
      Seq("")
    else {
      for {
        windowSize <- s.length.to(1, -1)
        window <- s.sliding(windowSize)
      } yield window
    }
  }

  def all_matching_substrings_naive(s1: String, s2: String): IterableOnce[String] = {
    var number_of_comparisons = 0L
    for {
      ss1 <- all_substrings(s1).iterator
      ss2 <- all_substrings(s2).iterator
      _ = number_of_comparisons = number_of_comparisons + 1
      if ss1 == ss2
    } yield {
      println(s"$number_of_comparisons comparisons")
      ss1
    }
  }


  def all_hashes_and_positions(s:String, windowLen:Int): IterableOnce[(Int, Int)] = {
    val p = 1000000007

    // first, generate an array
    val powers = Array.ofDim[Long](26)
    powers(0) = 1
    for { i <- 1 until 26} {
      powers(i) = math.floorMod(powers(i - 1) * 26, p)
    }

    for {i <- 0 until (s.length - windowLen)}
    yield {
      ???
    }

  }


  def main(args: Array[String]) = {
    val s1 = "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzkkkkkkkkkkkkkkkkkkkkkkkkkkkkkzzzzzzzzzzzzzzzzzzzzzztest1abc"
    val s2 = "brajjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjabcka"

    println(
      all_matching_substrings_naive(s1, s2)
        .iterator
        .take(1)
        .mkString("\n")
    )
  }

}
