object InvertBinaryTree {

  enum Tree[AA]:
    case Leaf[A](a: A) extends Tree[A]
    case Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  import Tree._

  def invert[A](tree: Tree[A]): Tree[A] =
    tree match
      case leaf@Leaf(a) =>      leaf
      case Node(left, right) => Node(invert(right), invert(left))


  val examples = Seq(
    Node(
      Node(
        Leaf(0),
        Leaf(1)),
      Node(
        Leaf(2),
        Leaf(3)
      )
    ),
    Node(
      Node(
        Leaf(0),
        Node(
          Leaf(1),
          Leaf(2))),
      Leaf(3)
    )
  )

  def main(args: Array[String]): Unit =
    for example <- examples do
      val reversed = invert(example)
      println(reversed)
}
