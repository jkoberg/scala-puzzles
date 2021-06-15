import scala.collection._

object DiGraph extends App {

  // Given a directed graph, design an algorithm to find out whether there is a route between 2 nodes.

  /**
   *  Each node of the graph has edges to other nodes and stores some arbitrary value
   * @param a The value at this node
   * @param edges The edges leading to other nodes
   * @tparam A The type of values stored in this graph
   */
  case class Node[A](
    a: A,
    edges: Seq[Node[A]] = Seq.empty
  )

  /**
   * New nodes discovered as the graph is traversed must be recorded with the path that led us there.
   * @param node A node that remains unvisited
   * @param via the path taken to get to `node`
   * @tparam A The type of values stored in this graph
   */
  case class WillVisit[A](
    node: Node[A],
    via: Seq[Node[A]] = Seq.empty
  )


  def doesPathExist[A](
                        current: Node[A],
                        destination: Node[A],
                        pathFromOrigin: Seq[Node[A]] = Seq.empty,
                        alreadyVisited: mutable.Set[Node[A]] = mutable.Set.empty[Node[A]]
                      ): Boolean = {
    val pathSoFar = pathFromOrigin :+ current
    if (current.edges.contains(destination)) {
      println(s"Path found: ${(pathSoFar :+ destination).map(_.a.toString).mkString(" -> ")}")
      true
    } else {
      alreadyVisited.add(current)
      val toVisit = current.edges.filter(edge => !alreadyVisited.contains(edge))
      toVisit.exists(v => doesPathExist(v, destination, pathSoFar, alreadyVisited))
    }
  }

  def shortestPath[A](toVisit: mutable.Queue[WillVisit[A]], destination: Node[A]): Option[Seq[Node[A]]] = {
    val visited: mutable.Set[Node[A]] = mutable.Set.empty

    while (toVisit.nonEmpty) {
      val current = toVisit.dequeue()
      val newVia = current.via :+ current.node
      if (current.node == destination) {
        println(s"Path found: ${newVia.map(_.a.toString).mkString(" -> ")}")
        return Some(newVia)
      } else {
        visited.add(current.node)
        toVisit.enqueueAll(
          current.node.edges
            .filter(!visited.contains(_))
            .map(WillVisit(_, newVia))
        )
      }
    }
    None
  }

  def shortestPath[A](origin: Node[A], destination: Node[A]): Option[Seq[Node[A]]] = {
    shortestPath(mutable.Queue(WillVisit(origin)), destination)
  }


  val C = Node("C")
  val F = Node("F")
  val G = Node("G")
  val B = Node("B", Seq(C))
  val E = Node("E", Seq(F))
  val D = Node("D", Seq(E))
  val A = Node("A", Seq(B, D, G, E))

  def test1(): Unit = {
    // A --> B --> C
    // |--> D --> E --> F
    // |--> G    |
    // |---------+

    println("test a -> e")
    assert(doesPathExist(A, E) == true)
    assert(doesPathExist(A, F) == true)
    assert(doesPathExist(A, C) == true)
    println("test e -> g")
    assert(doesPathExist(E, G) == false)
    println("Test 1 Finished successfully")

  }

  test1()

  def test2(): Unit = {
    println("test a -> e")
    assert(shortestPath(A, E).nonEmpty)
    assert(shortestPath(A, F).nonEmpty)
    assert(shortestPath(A, C).nonEmpty)
    println("test e -> g")
    assert(shortestPath(E, G).isEmpty)
    println("Test 2 Finished successfully")

  }

  test2()

}
