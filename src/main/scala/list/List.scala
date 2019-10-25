package list

/*
 * Abstract data type: https://en.wikipedia.org/wiki/List_(abstract_data_type)
 * functional-data-structures project
 * by Giuseppe Lorusso (call me Peppo)
 * about.me/giuseppelorusso
 */

sealed trait List[+A] {
  def isEmpty: Boolean
  def prepend[B >: A](item: B): List[B]
  def append[B >: A](item: B): List[B]
  def head: A
  def tail: List[A]
}

object List {
  // Exceptions------------------------------------------------------------|
  class EmptyListException(msg: String) extends RuntimeException(msg)
  //-----------------------------------------------------------------------|

  case object Nil extends List[Nothing] {
    override def isEmpty: Boolean = true

    override def prepend[B >: Nothing](item: B): List[B] = Cons(item, Nil)

    override def append[B >: Nothing](item: B): List[B] = Cons(item, Nil)

    override def head: Nothing = throw new EmptyListException("empty list")

    override def tail: List[Nothing] = throw new EmptyListException("empty list")
  }

  case class Cons[A](h: A, t: List[A]) extends List[A] {
    override def isEmpty: Boolean = false

    override def prepend[B >: A](item: B): List[B] =
      Cons(item, Cons(h,t))

    override def append[B >: A](item: B): List[B] =
      Cons(h,t.append(item))

    override def head: A = h

    override def tail: List[A] = t
  }

  def apply[A](xs: A*): List[A] = {
    var l: List[A] = Nil
    for(x <- xs) l = l.append(x)
    l
  }

}
