package org.greent
import scala.reflect.ClassTag

/**
  * Sorting Algorithms Analyses
  *
  * @author Filipe Gonzaga Miranda
  */
object SortingAlgs {


  /**
    * This version uses one single function that does the sorting and the merging
    *
    * Please note an imperative style is intentionally chosen here.
    * For experimental purposes. At the same time, "whiles" are avoided, and for expressions are used with ranges
    *
    * @param a
    * @return
    */
  def mergeSort(a: Array[Int]): Array[Int] = {
    val arrSize = a.length
    if (arrSize == 1) {
      return a
    }
    val firstArrSize = arrSize / 2
    val secArrSize = arrSize / 2 + (arrSize % 2)
    val a1 = new Array[Int](firstArrSize)
    val a2 = new Array[Int](secArrSize)

    val upTo = (arrSize / 2)
    for (i <- 0 until upTo) {
      a1(i) = a(i)
    }
    var pos = 0
    for (i <- upTo to arrSize - 1) {
      a2(pos) = a(i)
      pos += 1
    }

    val aR1 = mergeSort(a1)
    val aR2 = mergeSort(a2)
    val aFinalR = new Array[Int](arrSize)

    var i = 0
    var j = 0
    var upBoundR1 = false;
    var upBoundR2 = false;
    for (k <- 0 until arrSize) {
      if (aR1(i) < aR2(j) && !upBoundR1) {
        aFinalR(k) = aR1(i)
        if (i == aR1.length - 1) {
          upBoundR1 = true
        }
        else {
          i = i + 1
        }
      } else if (aR2(j) < aR1(i) && !upBoundR2) {
        aFinalR(k) = aR2(j)
        if (j == aR2.length - 1) {
          upBoundR2 = true
        }
        else {
          j = j + 1
        }
      } else {
        if (upBoundR1) {
          aFinalR(k) = aR2(j)
          j = if (j == aR2.length - 1) j else j + 1
        } else {
          aFinalR(k) = aR1(i)
          i = if (i == aR1.length - 1) j else i + 1
        }
      }
    }
    return aFinalR
  }

  /**
    * Trait for adding and decorating java.lang.Comparable types with syntatic sugar Greater and Less Methods = >  and  <
    * Used then by implicit conversion
    * @tparam T
    */
  trait SComparable[T] extends Comparable[T] {
    def >(other: T): Boolean = {
      val i = this.compareTo(other)
      if (i > 0) true else if (i < 0) false else false
    }
    def <(other: T): Boolean = {
      !this.>(other)
    }
  }

  /**
    * Implicit conversion from Comparable, such as java Integers and Strings to SComparable, SComparable, decorates
    * those Java Comparables with syntatic sugar methods fro comparing
    * @param c a java.lang.Comparable
    * @tparam T the type of comparable
    * @return an SComparable
    */
  implicit def >[T](c: Comparable[T]): SComparable[T] = {
    new SComparable[T] {
      def compareTo(other: T): Int = {
        c.compareTo(other)
      }
    }
  }

  /**
    * This version is a little less verbose, and uses an local function to merge
    * the arrays
    *
    * @param a
    * @tparam T
    * @return
    */
  def mergeSortIn2Funs[T <: Comparable[T]](a: Array[T])(implicit m: ClassTag[T]): Array[T] = {
    val arrSize = a.length
    if (arrSize == 1) return a
    val (left, right) = a.splitAt(arrSize / 2)
    def merge(left: Array[T], right: Array[T])(implicit m: ClassTag[T]) = {
      val arrResult = new Array[T](arrSize)
      var k = 0
      var l = 0
      var r = 0
      val lLength = left.length
      val rLength = right.length

      while ((l < lLength || r < rLength) && !(k == arrResult.length)) {
        val lEnded = (l == lLength)
        val rEnded = (r == rLength)
        if ( !(lEnded || rEnded) && left(l) < right(r)) {
          arrResult(k) = left(l)
          k += 1
          l += 1
        }
        else if (!(lEnded || rEnded) && right(r) < left(l)) {
          arrResult(k) = right(r)
          k += 1
          r += 1
        } else {
          if (l < lLength) {
            arrResult(k) = left(l)
            l += 1
            k += 1
          } else if (r < rLength) {
            arrResult(k) = right(r)
            r += 1
            k += 1
          }
        }
      }
      arrResult
    }
    merge(mergeSortIn2Funs(left), mergeSortIn2Funs(right))
  }

}
