

object EvaluateDivision {

  def calcEquation(equations: List[List[String]], values: Array[Double], queries: List[List[String]]): Array[Double] = {
    import scala.util.boundary
    val edges: collection.mutable.Map[String, collection.mutable.Map[String, Double]] = collection.mutable.Map.empty

    def add(start:String, end:String, value: Double): Unit = {
      edges.getOrElseUpdate(start, collection.mutable.Map.empty).addOne(end -> value)
    }

    def search(start: String, end: String): Seq[(String, Double)] = {
      if(edges.contains(start) && start == end) return Seq((start, 1.0)) // doh. query = "aa" = 1.0, query = "??" = -1.0
      val toVisit = collection.mutable.Queue[(String, Seq[(String, Double)])]((start, Seq.empty))
      val visited = collection.mutable.Set.empty[String]

      boundary {
        while (toVisit.nonEmpty) {
          val (current, path) = toVisit.dequeue()
          visited.addOne(current)
          for {
            (nkey, nvalue) <- edges.getOrElse(current, collection.mutable.Map.empty)
            if !visited.contains(nkey)
          } {
            val newPath = path :+ (nkey, nvalue)
            if (nkey == end) {
              boundary.break(newPath)
            }
            toVisit.enqueue((nkey, newPath))
          }
        }
        Seq.empty
      }
    }

    for {
      case (List(a, b), v) <- equations.zip(values)
    } {
      add(a, b, v)
      add(b, a, 1/v)
    }

    val output = Array.fill[Double](queries.length)(-1)

    for {
      case (List(c, d), j) <- queries.zipWithIndex
    } yield {
      val path = search(c, d)
      if(path.nonEmpty) {
        output(j) = path.map(_._2).product
      }
    }

    output
  }


}
