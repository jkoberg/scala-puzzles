import scala.collection.mutable

object LargestIslandSize {

  def exploreNeighborhood(grid: Array[Array[Int]], startRowIdx: Int, startColIdx: Int): Int = {
    val toVisit = mutable.Stack.empty[(Int, Int)]
    // start from the given initial coordinates
    toVisit.push((startRowIdx, startColIdx))
    var found = 0
    while(toVisit.nonEmpty) {
      val (rowIdx, colIdx) = toVisit.pop()
      // Do nothing if out of bounds, or cell to examine has already been visited or is not "land",
      if (!(rowIdx < 0 || rowIdx >= grid.length || 
            colIdx < 0 || colIdx >= grid(rowIdx).length || 
            grid(rowIdx)(colIdx) == 0)) {
        found = found + 1
        // Since we counted this cell, mark it zero to avoid it being considered again.
        grid(rowIdx)(colIdx) = 0
        toVisit.pushAll(Seq(
          (rowIdx, colIdx + 1),
          (rowIdx, colIdx - 1),
          (rowIdx + 1, colIdx),
          (rowIdx - 1, colIdx)
        ))
      }
    }
    found
  }

  def largestIslandSize(grid: Array[Array[Int]]): Int = {
    var largestFound = 0
    for {
      rowIdx <- grid.indices
      colIdx <- grid(rowIdx).indices
      if grid(rowIdx)(colIdx) == 1
    } {
      largestFound = largestFound.max(exploreNeighborhood(grid, rowIdx, colIdx))
    }
    largestFound
  }

  def main(args: Array[String]): Unit = {
    val test1 = Array(
      Array(1, 0, 1, 1, 0),
      Array(0, 0, 1, 1, 0),
      Array(1, 1, 0, 1, 1),
      Array(1, 0, 0, 0, 0)
    )

    val result = largestIslandSize(test1)
    println(s"Result: $result")
  }
}

