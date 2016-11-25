package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genMap: Gen[Map[Int,Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int,Int]), genMap)
  } yield m.updated(k, v)

  lazy val genHeap: Gen[H] = {
    val seq = for {
      v <- arbitrary[Int]
      h <- insert(v, empty)
      h2 <- insert(v, h)
    } yield h
    seq
  }


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

//  ensures that if you insert an element into an empty heap, then find the minimum of the resulting heap, you get the element back:
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  //If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, empty)
    val h2 = insert(b, h)

    if (a < b) findMin(h2) == a
    else if(a > b) findMin(h2) == b
    else findMin(h2) == a && findMin(h2) == b
  }

  //  If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.
  property("insert1") = forAll { a: Int =>
    val h = insert(a, empty)
    val h2 = deleteMin(h)
    isEmpty(h2)
  }

  //test deletion results in a smaller list
  property("del") = forAll { a: Int =>
    val h = insert(a, empty)
    val h2 = insert(a + a, h)
    val h3 = deleteMin(h2)
    findMin(h3) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  //  Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("meld1") = forAll { (h1: H, h2: H)=>

    if (isEmpty(h1) || isEmpty(h2)) true
    else  {
      val h = meld(h1, h2)
      val m = findMin(h)
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      if (m > m1 || m > m2) false
      else true
    }

  }

  def isSorted(prev: Int, heap: H, sorted: Boolean): Boolean = {
    if(isEmpty(heap)) sorted
    else {
      val m = findMin(heap)
      if(prev <= m) isSorted(m, deleteMin(heap), true)
      else false
    }
  }
//  Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
  property("sort1") = forAll { (h: H) =>
    if (isEmpty(h)) true
    else {
      val m = findMin(h)
      isSorted(m, deleteMin(h), true)
    }

  }
}
