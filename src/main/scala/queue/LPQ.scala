package queue

/*
 * Abstract data type: https://en.wikipedia.org/wiki/Priority_queue
 * (NOTE: this implementation avoid duplicate items in the queue, just to do more practice)
 * functional-data-structures project
 * by Giuseppe Lorusso (call me Peppo)
 * about.me/giuseppelorusso
 */

sealed trait PriorityQueue[+A] {
  def isNew: Boolean
  def insert[B >: A](item: B, priority: Integer)(implicit ordering: Ordering[B]): PriorityQueue[B]
  def getFirst: Option[A]
  def delFirst: PriorityQueue[A]
  def changePriority[B >: A](item: B, priority: Integer)(implicit ordering: Ordering[B]): PriorityQueue[B]
  def getPriority[B >: A](item: B): Option[Integer]
  def size: Integer
}


object LPQ {

  case class Record[+A](item: A, priority: Integer, next: Record[A] = null){
    def inject[B >: A](new_item: B, new_priority: Integer)(implicit ordering: Ordering[B]): Record[B] = this match {
      case null =>
        Record(item, priority, next)
      case Record(i, _, _) if ordering.compare(i, new_item) == 0 =>
        throw new IllegalArgumentException(s"$item duplicate key")
      case Record(i, p, r) if new_priority < p =>
        Record(new_item, new_priority, Record(i, p, r))
      case Record(i, p, r) if new_priority >= p =>
        if (r != null) Record(i, p, r.inject(new_item, new_priority))
        else Record(i, p, Record(new_item, new_priority, null))
    }

    override def toString: String = this match {
      case null => ""
      case Record(i, p, null) => s"$i - $p"
      case Record(i, p, r) => s"$i - $p\n$r "
    }
  }


  class LinkedPriorityQueue[A](head: Record[A] = null) extends PriorityQueue[A]{

    override def isNew: Boolean =
      head == null

    override def insert[B >: A](item: B, priority: Integer)(implicit ordering: Ordering[B]): LinkedPriorityQueue[B] = {
      if (head == null) new LinkedPriorityQueue(Record(item, priority))
      else new LinkedPriorityQueue(head.inject(item, priority))
    }

    override def getFirst: Option[A] = head match {
      case null => None
      case _ => Option(head.item)
    }

    override def delFirst: LinkedPriorityQueue[A] =
      if (this.isNew) this
      else new LinkedPriorityQueue[A](head.next)

    override def changePriority[B >: A](item: B, priority: Integer)(implicit ordering: Ordering[B]): LinkedPriorityQueue[B] = head match {
      case Record(i, _, _) if i == item => this.delFirst.insert(item, priority)
      case Record(i, p, next) => new LinkedPriorityQueue[A](next).changePriority(item, priority).insert(i, p)
      case null => throw new IllegalArgumentException("Item not found.")
    }

    override def getPriority[B >: A](item: B): Option[Integer] = head match {
      case null => None
      case Record(i,_,_) if i == item => Option(head.priority)
      case _ => new LinkedPriorityQueue[A](head.next).getPriority(item)
    }

    override def size: Integer =
      if (head == null) 0
      else 1 + new LinkedPriorityQueue[A](head.next).size

    override def toString: String =
      if (isNew) "empty"
      else head.toString
  }

  def empty[A]: LinkedPriorityQueue[A] = new LinkedPriorityQueue[A]

  def apply[A](xs: (A, Integer)*)(implicit comparator: Ordering[A]): LinkedPriorityQueue[A] =
    xs.foldLeft(LPQ.empty[A]){ case (acc, x) => acc.insert(x._1, x._2) }
}