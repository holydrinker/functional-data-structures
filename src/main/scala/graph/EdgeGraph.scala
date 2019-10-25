package graph

/*
 * Abstract data type: https://en.wikipedia.org/wiki/Graph_(abstract_data_type)
 * functional-data-structures project
 * by Giuseppe Lorusso (call me Peppo)
 * about.me/giuseppelorusso
 *
 * This graph implementation manages edges directly and nodes indirectly.
 * Sometimes can be more usable.
 */

object EdgeGraph {

  case class EdgeGraph[+A](edges: List[(A, A)]) extends NodelessGraph[A] {

    override def adjacent[B >: A](x: B, y: B): Boolean =
      edges.count {
        case (n1, n2) if n1 == x && n2 == y => true
        case (n1, n2) if n1 == y && n2 == x => true
        case (_, _) => false
      } > 0

    override def neighbors[B >: A](x: B): List[B] =
      edges
        .filter(_.productIterator.toSet.contains(x))
        .flatMap(_.productIterator.toList)
        .filter(_ != x)
        .asInstanceOf[List[B]]

    override def removeVertex[B >: A](x: B): EdgeGraph[B] =
      EdgeGraph(
        edges.filter(!_.productIterator.toSet.contains(x))
      )


    override def addEdge[B >: A](x: B, y: B): EdgeGraph[B] =
      EdgeGraph(
        edges ::: List(x -> y)
      )

    override def setVertex[B >: A](oldVertex: B, newVertex: B): EdgeGraph[B] = {
      val changing = edges.filter(_.productIterator.toSet.contains(oldVertex))
      val unchanging = edges.filter(!_.productIterator.toSet.contains(oldVertex))

      val changed =
        changing.map {
          _.productIterator.toList.map {
            x =>
              if (x == oldVertex) newVertex
              else x
          }
        }.asInstanceOf[List[(B, B)]]

      EdgeGraph(unchanging ::: changed)
    }

    override def adjacentMatrix: List[List[Int]] = {
      val nodes = edges.flatMap(_.productIterator.toList).distinct

      val flattenMatrix =
        for {
          n1 <- nodes
          n2 <- nodes
          value = if (adjacent(n1, n2)) 1 else 0
        } yield value

      flattenMatrix.grouped(nodes.size).toList
    }
  }

  def apply[A](): EdgeGraph[A] =
    EdgeGraph(Nil)
}