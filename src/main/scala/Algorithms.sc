

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

//val sorted = mergeSort(Array(2,5,1,9,3,0,99, -1))