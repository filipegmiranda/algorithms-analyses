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
        } else if (left(i) < right(j)) {
          aR(k) = left(i)
          i += 1
          lE = i == left.length
        } else if (left(i) > right(j)) {
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

val r0 = sortAndCountInv(Array(6, 5, 4, 3, 2, 1))._2

def inv(list : List[Int]) : Int = doInv(list)._1

def doInv(list : List[Int]) : (Int, List[Int]) =
  if (list.length <= 1) {
    (0, list)
  } else {
    val (left, right) = list.splitAt(list.length / 2)
    val (leftCount, leftList) = doInv(left)
    val (rightCount, rightList) = doInv(right)
    val (mergeCount, mergeList) = doMerge(leftList, rightList)
    (leftCount + rightCount + mergeCount, mergeList)
  }

def doMerge(left : List[Int], right : List[Int], count : Int = 0) : (Int, List[Int]) =
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


var r = inv(List(6,5,4,3,2,1))




