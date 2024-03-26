object ArrayProductExcept {

  /**
   * For each array element, compute the product of all the other array elements.
   *
   * For each array element a(i), find product(a(0..i-1)) * product(a(i+1..n))
   * To do this efficiently, pre-fill the output array starting from the right, accumulating
   * the product of each successive input element (from the right). This yields an array, where,
   * for each output element, the product of all the successive input elements are in the output element
   * to its right.
   *
   * Then, we traverse the input and accumulate successive products from the left, computing the final
   * product for each output element as we go, and then overwriting the previously-computed partial product
   * as it's no longer needed.
   * @param input An array of ints, of at least length 2, where the product of any suffix or prefix is < 2**31
   * @return An array of ints, where each element is the product of all input elements EXCEPT the one at
   *         the element's index.
   */
  def arrayProductExcept(input: Array[Int]): Array[Int] = {
    // No need to initialize or copy to the output array since we'll be overwriting everything
    val output = Array.ofDim[Int](input.length)
    var acc = 1;

    for
      i <- input.indices.reverse
    do
      acc = acc * input(i)
      output(i) = acc

    acc = 1
    for
      // stop one short of the end, since the last element is a special case
      i <- 0.to(output.length - 2)
    do
      output(i) = acc * output(i + 1)
      acc = acc * input(i)

    // handle the last element
    output(output.length - 1) = acc
    output
  }

  def test(arr: Array[Int]): Unit = {
    println(arrayProductExcept(arr).map(_.toString).mkString(", "))
  }

  def main(args: Array[String]): Unit = {
    test(Array(2,3,4,5,6))
    test(Array(1, 1, 1, 1, 1))
    test(Array(2, 0, 2, 2, 2))
    test(Array(2, -5, -2, 2, 2))
    test(Array(7, 11))
    val hugeArray = Array.tabulate[Int](100_000_000)(_ % 2)
    arrayProductExcept(hugeArray) // Run this once to warm up the JIT
    val t0 = System.nanoTime()
    val output = arrayProductExcept(hugeArray)
    val t1 = System.nanoTime()
    val td = (t1 - t0)
    val td_secs = td / 1e9
    val interval_ns = td.toDouble / hugeArray.length
    val rate_m_per_sec = (hugeArray.length / 1e6) / td_secs
    println(f"100M elements, $td_secs%.3f seconds, $interval_ns%.3fns/ea, $rate_m_per_sec%.3fM/sec")
    println(s"Last item: ${output.last}")
  }
}
