package org.greent

import scala.annotation.tailrec

/**
  * Created by Filipe Gonzaga Miranda
  */
object GeneralAlgsForAnalysesBigO {

  /**
    * This small function returns true if an element of type T passed as first parameter is present in
    * the Array, false otherwise
    * This function has a running time of O(n)
    *
    * Or, we can say that the running time of this algorithm is linear on its input n
    *
    * @param e the element to search for
    * @param a the array to look up the element
    * @tparam T the type of element
    * @return true if the element is present in the array, false otherwise
    */
  def existsElement[T](e: T, a: Array[T]): Boolean = {
    @tailrec
    def loop[T](n: Int, a: Array[T]): Boolean = {
      if (a(n) == e) true
      else if (n == a.length - 1) false
      else loop(n + 1, a)
    }
    loop(0, a)
  }


  /**
    * Tells if there is an common element in those 2 arrays
    *
    * The running time of this algorithm is Big Oh of n -> O(nˆ2)
    * or
    * The running time is quadratic in the input length n
    *
    * @param e  the element to be compared
    * @param a1 the first array to search for the element
    * @param a2 the second array to search for the element
    * @tparam T the Type of array and element
    * @return true of there is a common element in the array, false otherwise
    */
  def commonElement[T](e: T, a1: Array[T], a2: Array[T]): Boolean = {
    for (i <- 0 until a1.length) {
      for (j <- 0 until a2.length) {
        if (a1(i) == a2(j)) return true
      }
    }
    false
  }


  /**
    * Finds if an array has duplicate elements
    *
    * @param a the array to iterate through
    * @tparam T Is the type parameter os the array
    * @return true if the array has duplicates elements, false otherwise
    */
  def duplicateElements[T](a: Array[T]): Boolean = {
    for (i <- 0 until a.length) {
      for (j <- i + 1 until a.length) {
        if (a(i) == a(j)) return true
      }
    }
    false
  }


  def bruteForceCountNrInversions(a: Array[Int]): Int = {
    var nr = 0
    for (i <- 0 until a.length) {
      for (j <- i + 1 until a.length) {
        if (a(i) > a(j)) {
          nr += 1
        }
      }
    }
    nr
  }

  /**
    * Extremely imperative style of doing a "count nr of inversions"
    *
    * runs in O( n log n )
    *
    * @param a the array of nr
    * @return the number of inversions and a sorted array
    */
  def sortAndCountInv(a: Array[Int]): (Array[Int], Int) = {
    if (a.length == 1) {
      return (a, 0)
    } else {
      val (left, right) = a.splitAt(a.length / 2)
      val (sortedL, nrL) = sortAndCountInv(left)
      val (sortedR, nrR) = sortAndCountInv(right)
      def mergeAndCountInv(left: Array[Int], right: Array[Int], n: Int): (Array[Int], Int) = {
        val aR = new Array[Int](n)
        var nr = 0
        var i = 0
        var j = 0
        var rE = false
        var lE = false
        for (k <- 0 until n) {
          if (lE) {
            aR(k) = right(j)
            j += 1
            nr += 1
          } else if (rE) {
            aR(k) = left(i)
            i += 1
            nr = nr + 1 + (left.length - 1) - 1
          } else if (left(i) < right(j)) {
            aR(k) = left(i)
            i += 1
            nr += nr + 1 + (left.length - 1) - 1
            lE = i == left.length
          } else if (right(j) < left(i)) {
            aR(k) = right(j)
            j += 1
            rE = j == right.length
            nr += 1
          } else {
            aR(k) = left(i)
            i += 1
            lE = i == left.length
          }
        }
        (aR, nr)
      }
      val fR = mergeAndCountInv(sortedL, sortedR, sortedL.length + sortedR.length)
      (fR._1, fR._2 + nrL + nrR)
    }
  }


  /**
    * Functional way of counting nr of inversions through the use of pattern matching
    * runs in O( n log n )
    * @param list
    * @return
    */
  def doInv(list: List[Int]): (Int, List[Int]) = {
    if (list.length <= 1) {
      (0, list)
    } else {
      val (left, right) = list.splitAt(list.length / 2)
      val (leftCount, leftList) = doInv(left)
      val (rightCount, rightList) = doInv(right)
      val (mergeCount, mergeList) = doMerge(leftList, rightList)
      (leftCount + rightCount + mergeCount, mergeList)
    }
    def doMerge(left: List[Int], right: List[Int], count: Int = 0): (Int, List[Int]) =
      (left, right) match {
        case (Nil, r) => (count, r)
        case (l, Nil) => (count, l)
        case (lhead :: ltail, rhead :: rtail) =>
          if (lhead <= rhead) {
            val (lcount, list) = doMerge(ltail, right, count)
            (count + lcount, lhead :: list)
          } else {
            val (rcount, list) = doMerge(left, rtail, count)
            (count + left.length + rcount, rhead :: list)
          }
      }
  }

}

