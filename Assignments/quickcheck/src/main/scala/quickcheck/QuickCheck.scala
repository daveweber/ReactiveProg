package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, value(empty)), (9, genHeap))
  } yield insert(n, h)
  
  property("emptyHeapInserts") = forAll { (a1: A, a2: A) =>
    val h = insert(a1, insert(a2, empty))
    findMin(h) == min(a1, a2)   
  }
  
  property("insertDelete") = forAll { a1: A =>
    val h = deleteMin(insert(a1, empty))
    isEmpty(h)  
  }
  
  property("sortedDeletes") = forAll { h: H =>    
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val min = findMin(h)
        val h2 = deleteMin(h)
        isEmpty(h2) || (min <= findMin(h2) && isSorted(h2))
      }
      
    isSorted(h)
  }
  
  property("minFromMeld") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))    
  }
  
  // Idea from https://class.coursera.org/reactive-001/forum/thread?thread_id=97
  property("meldHeapMins") = forAll { (h1: H, h2: H) =>
    def isEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val min1 = findMin(h1)
        val min2 = findMin(h2)
        min1 == min2 && isEqual(deleteMin(h1), deleteMin(h2))
      }
      
    isEqual(
        meld(h1,h2), 
        meld(deleteMin(h1), insert(findMin(h1), h2)))
  }
  
  


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
