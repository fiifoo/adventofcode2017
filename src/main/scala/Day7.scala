import scala.io.Source

object Day7 {
  def input: Iterator[String] = Source.fromResource("day7.txt").getLines

  def one: Id = {
    val (nodes, parents) = read

    root(nodes, parents)
  }

  def two: Int = {
    val (nodes, parents) = read
    var result: Option[Int] = None

    def calculateResult(children: List[Id], weights: List[Int]): Int = {
      val (a, b) = weights partition (_ == weights.head)
      val (correct, wrong) = if (a.lengthCompare(1) == 0) (b.head, a.head) else (a.head, b.head)
      val fix = correct - wrong

      val (culprit, _) = (children zip weights find (_._2 == wrong)).get

      nodes(culprit).weight + fix
    }

    def process(id: Id): Int = {
      val node = nodes(id)
      val weights = node.children map process

      if (result.isEmpty && weights.toSet.size == 2) {
        result = Some(calculateResult(node.children, weights))
      }

      node.weight + weights.sum
    }

    process(root(nodes, parents))

    result.get
  }

  private def root(nodes: Map[Id, Node], parents: Map[Id, Id]): Id = {
    val result = nodes.values find (node => !parents.isDefinedAt(node.id)) map (_.id)

    result.get
  }

  private def read: (Map[Id, Node], Map[Id, Id]) = {
    val nodes = (input map Node.read map (node => node.id -> node)).toMap

    val parents = (nodes.values flatMap (node => {
      (node.children map (child => child -> node.id)).toMap
    })).toMap

    (nodes, parents)
  }

  type Id = String

  object Node {
    private val pattern = "(\\w+) \\((\\d+)\\)(?: -> (.+))?".r

    def read(source: String): Node = {
      val pattern(id, weight, children) = source

      Node(
        id,
        weight.toInt,
        if (children == null) List() else children.split(", ").toList
      )
    }
  }

  case class Node(id: Id, weight: Int, children: List[Id])

}
