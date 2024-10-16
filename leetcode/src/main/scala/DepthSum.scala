object DepthSum {

  def depthSum(items: Seq[Any], depth:Int = 1): Int = {
    var sum = 0
    for {
      item <- items
    } {
      item match {
        case number: Int =>
          sum += depth * number
        case sublist: Seq[_] =>
          sum += depthSum(sublist, depth + 1)
      }
    }
    sum
  }

  def main(args: Array[String]): Unit = {
    val items = Seq(1, Seq(2, Seq(5, 6)), 4)
    println(depthSum(items))
  }

}
