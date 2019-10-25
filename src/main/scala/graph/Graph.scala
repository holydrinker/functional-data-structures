package graph

/*
 * Abstract data type: https://en.wikipedia.org/wiki/Graph_(abstract_data_type)
 * functional-data-structures project
 * by Giuseppe Lorusso (call me Peppo)
 * about.me/giuseppelorusso
 */

trait Graph[+A] extends NodelessGraph[A] {

  def addVertex[B >: A](x: B): Graph[B]

}

trait NodelessGraph[+A] {

  def adjacent[B >: A](x: B, y: B): Boolean

  def neighbors[B >: A](x: B): List[B]

  def removeVertex[B >: A](x: B): NodelessGraph[B]

  def addEdge[B >: A](x: B, y: B): NodelessGraph[B]

  def setVertex[B >: A](oldVertex: B, newVertex: B): NodelessGraph[B]

  def adjacentMatrix: List[List[Int]]

}