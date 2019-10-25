package sort

object Sort {

  private def isSorted(xs: List[Int]): Boolean =  xs match {
    case Nil => true
    case _ :: Nil => true
    case h :: t if h < t.head => isSorted(t)
    case _ => false
  }

  def bubblesort(xs: List[Int]): List[Int] = {
    def oneRound(xs: List[Int]): List[Int] = xs match {
      case Nil => Nil
      case List(x) => List(x)
      case List(x,y) if x < y => List(x,y)
      case List(y,x) if x > y => List(y,x)
      case h :: t if h > t.head => t.head :: oneRound(h :: t.tail)
      case h :: t => h :: oneRound(t)
    }

    if(isSorted(xs)) xs
    else bubblesort(oneRound(xs))
  }

  def quicksort(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case h :: Nil => List(h)
    case h :: t => quicksort(t.filter(_ < h)) ::: List(h) ::: quicksort(t.filter(_ >= h))
  }

}
