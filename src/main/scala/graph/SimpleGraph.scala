package graph

/*
 * Abstract data type: https://en.wikipedia.org/wiki/Graph_(abstract_data_type)
 * functional-data-structures project
 * by Giuseppe Lorusso (call me Peppo)
 * about.me/giuseppelorusso
 */

object SimpleGraph {

  case class SimpleGraph[+A](nodes: List[A], edges: List[(A, A)]) extends Graph[A] {

    override def adjacent[B >: A](x: B, y: B): Boolean =
      edges.count {
        case (n1, n2) if n1 == x && n2 == y => true
        case (n1, n2) if n1 == y && n2 == x => true
        case (_, _) => false
      } > 0

    override def neighbors[B >: A](x: B): List[B] =
      nodes.filter(adjacent(x, _))

    override def addVertex[B >: A](x: B): SimpleGraph[B] =
      if (nodes.contains(x))
        this
      else
        SimpleGraph(nodes ::: List(x), edges)

    override def removeVertex[B >: A](x: B): SimpleGraph[B] = {
      val newNodes = nodes.filter(_ != x)
      val newEdges =
        edges.filter {
          case (n1, n2) if n1 == x || n2 == x => false
          case (_, _) => true
        }

      SimpleGraph(newNodes, newEdges)
    }

    override def addEdge[B >: A](x: B, y: B): SimpleGraph[B] =
      if (nodes.contains(x) && nodes.contains(y))
        SimpleGraph(nodes, edges ::: List((x, y)))
      else
        this

    override def setVertex[B >: A](oldVertex: B, newVertex: B): SimpleGraph[B] =
      if (nodes.contains(oldVertex)) {
        val newNodes = nodes.map(x => if (x == oldVertex) newVertex else x)
        val newEdges =
          edges.map {
            case (n1, n2) if n1 == oldVertex => (newVertex, n2)
            case (n1, n2) if n2 == oldVertex => (n1, newVertex)
            case (n1, n2) => (n1, n2)
          }
        SimpleGraph(newNodes, newEdges)
      }

      else {
        this
      }

    override def adjacentMatrix: List[List[Int]] = {
      def adjacentValue(b: Boolean): Int =
        if (b) 1 else 0

      val flattenMatrix =
        for {
          n1 <- nodes
          n2 <- nodes
        } yield adjacentValue(adjacent(n1, n2))

      flattenMatrix.grouped(nodes.size).toList
    }
  }


  def apply[A](): SimpleGraph[A] =
    SimpleGraph(Nil, Nil)


}