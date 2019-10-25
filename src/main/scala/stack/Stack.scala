package stack

/*
 * Abstract data type: https://en.wikipedia.org/wiki/Stack_(abstract_data_type)
 * functional-data-structures project
 * by Giuseppe Lorusso (call me Peppo)
 * about.me/giuseppelorusso
 */

sealed trait Stack[+A] {
  def isEmpty: Boolean
  def top: A
  def push[B >: A](item: B): Stack[B]
  def pop: Stack[A]
}

object Stack {

  //Exception--------------------------------------------------|
  class EmptyStack(msg: String) extends RuntimeException(msg)
  //-----------------------------------------------------------|

  case object Empty extends Stack[Nothing] {
    override def isEmpty: Boolean = true

    override def top: Nothing = throw new EmptyStack("empty stack.")

    override def push[A](item: A): Stack[A] = NotEmpty(item, Empty)

    override def pop: Stack[Nothing] = throw new EmptyStack("empty stack.")
  }

  case class NotEmpty[A](h: A, t: Stack[A]) extends Stack[A] {
    override def isEmpty: Boolean = false

    override def top: A = h

    override def push[B >: A](item: B): Stack[B] = NotEmpty(item, NotEmpty(h,t))

    override def pop: Stack[A] = t
  }

  def empty[A]: Stack[A] = Empty

  def apply[A](xs: A*): Stack[A] = {
    // xs.foldLeft(empty){ case(acc, x) => acc.push(x) } //why this doesn't work?
    val s = Empty
    for(x <- xs.reverse) s push x
    s
  }
}
