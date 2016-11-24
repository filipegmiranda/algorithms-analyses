package org.greent

/**
  * Sorting Algorithms Analyses
  *
  * @author Filipe Gonzaga Miranda
  */
object SortingAlgs {


  def mergeSort(a: Array[Int]): Array[Int] = {
    val size = a.length

    if (size == 1) {
      return a
    }
    val a1 = new Array[Int](size / 2)
    val a2 = new Array[Int](size / 2)

    val upTo = (a.length / 2) - 1
    for (i <- 0 to upTo) {
      a1(i) = a(i)
    }
    var pos = 0
    for (i <- upTo + 1 to a.length - 1) {
      a2(pos) = a(i)
      pos += 1
    }

    val aR1 = mergeSort(a1)
    val aR2 = mergeSort(a2)
    val aFinalR = new Array[Int](size)

    var i = 0
    var j = 0
    var upBoundR1 = false;
    var upBoundR2 = false;
    for (k <- 0 to aFinalR.length - 1) {
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


}
