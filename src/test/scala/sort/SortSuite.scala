package sort
import org.scalatest.FunSuite
import sort.Sort.bubblesort

class SortSuite extends FunSuite {

  test ("bubble sort works on empty lists") {
    val xs = Nil
    val actual = bubblesort(xs)
    val expected = Nil
    assert(actual == expected)
  }

  test ("bubble sort works on one-item list") {
    val xs = List(1)
    val actual = bubblesort(xs)
    val expected = xs
    assert(actual == expected)
  }

  test ("bubble sort works on ordered two-items list") {
    val xs = List(1, 2)
    val actual = bubblesort(xs)
    val expected = xs
    assert(actual == expected)
  }

  test ("bubble sort works on unordered two-items list") {
    val xs = List(2, 1)
    val actual = bubblesort(xs)
    val expected = List(1, 2)
    assert(actual == expected)
  }

  test ("bubble sort works on n-items ordered list") {
    val xs = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val actual = bubblesort(xs)
    val expected = xs
    assert(actual == expected)
  }

  test ("bubble sort works on n-items reverse ordered list") {
    val xs = List(9, 8, 7, 6, 5, 4, 3, 2, 1)
    val actual = bubblesort(xs)
    val expected = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    assert(actual == expected)
  }

  test ("bubble sort works on n-duplicate-items list") {
    val xs = List(1, 1, 1, 1, 1, 1, 1)
    val actual = bubblesort(xs)
    val expected = xs
    assert(actual == expected)
  }

}
