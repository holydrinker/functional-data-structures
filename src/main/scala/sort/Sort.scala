package sort

object Sort {

  private def isSorted(xs: List[Int]): Boolean =  xs match {
    case Nil | _ :: Nil => true
    case h :: t if h <= t.head => isSorted(t)
    case _ => false
  }

  def bubblesort(xs: List[Int]): List[Int] = {
    def oneRound(xs: List[Int]): List[Int] = xs match {
      case h1 :: h2 :: t => if (h1 > h2) h2 :: oneRound(h1 :: t) else h1 :: oneRound(h2 :: t)
      case _ => xs
    }

    if(isSorted(xs)) xs
    else bubblesort(oneRound(xs))
  }

  def quicksort(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case h :: t => quicksort(t.filter(_ < h)) ::: List(h) ::: quicksort(t.filter(_ >= h))
  }

}
