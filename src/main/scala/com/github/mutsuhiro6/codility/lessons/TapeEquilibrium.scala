package com.github.mutsuhiro6.codility.lessons

/**
  * 100% achievement
  */
object TapeEquilibrium {

  /**
    * In the case A = (3,1,2,4,3).
    * 1. Calculate "difference" with P=5/2=2,
    * then the difference is (3+1)-(2+4+3)=-5.
    * 2. Update "difference" with P=P-1 and P=P+1 iteratively.
    * 2.1. The difference when P=P-1=1 is (4-1)-(9+1)=-5-1*2.
    * 2.2. The difference when P=P+1=3 is (4+2)-(9-2)=-5+2*2.
    *
    * Generally,
    * 1. Calculate "difference" (as diff) with P=N/2.
    * 2. Update "difference"s.
    * 2.1. When P=P-1, diff=diff-A[P]*2
    * 2.2. When P=P+1, diff=diff+A[P-1]*2
    * 2.3. Update minimum absolute difference.
    * 3. Repeat 2. for all P.
    *
    * Calculation cost is O(N/2)+O(N/2+m) (m is enough smaller than N).
    */
  def solution(a: Array[Int]): Int = {
    val n = a.length
    val p = n / 2
    var diff = 0
    var pp = n / 2
    var pm = n / 2
    for (i <- n - 1 to p by -1) {
      val l = {
        val j = n - 1 - i
        if (j > p - 1) 0
        else a(j)
      }
      val r = a(i)
      diff += l - r
    }
    if (diff == 0) return 0 // Minimum absolute diff is 0.
    var min = math.abs(diff)
    //    println(s"p: $p, abs diff: $min")
    var diff_p = diff
    var diff_m = diff
    while (pp < n || pm > 0) {
      pp += 1
      pm -= 1
      if (pp < n) {
        diff_p += a(pp - 1) * 2
        val absDiff_p = math.abs(diff_p)
        //        println(s"p: $pp, abs diff: $absDiff_p")
        if (min > absDiff_p) min = absDiff_p
      }
      if (pm > 0) {
        diff_m -= a(pm) * 2
        val absDiff_m = math.abs(diff_m)
        //        println(s"p: $pm, abs diff: $absDiff_m")
        if (min > absDiff_m) min = absDiff_m
      }
      //      println(s"min: $min")
      if (min == 0) return 0
    }
    min
  }
}

/**
  * A non-empty array A consisting of N integers is given. Array A represents numbers on a tape.
  * *
  * Any integer P, such that 0 < P < N, splits this tape into two non-empty parts: A[0], A[1], ..., A[P − 1] and A[P], A[P + 1], ..., A[N − 1].
  * *
  * The difference between the two parts is the value of: |(A[0] + A[1] + ... + A[P − 1]) − (A[P] + A[P + 1] + ... + A[N − 1])|
  * *
  * In other words, it is the absolute difference between the sum of the first part and the sum of the second part.
  * *
  * For example, consider array A such that:
  * *
  * A[0] = 3
  * A[1] = 1
  * A[2] = 2
  * A[3] = 4
  * A[4] = 3
  * We can split this tape in four places:
  * *
  * P = 1, difference = |3 − 10| = 7
  * P = 2, difference = |4 − 9| = 5
  * P = 3, difference = |6 − 7| = 1
  * P = 4, difference = |10 − 3| = 7
  * Write a function:
  * *
  * object Solution { def solution(a: Array[Int]): Int }
  * *
  * that, given a non-empty array A of N integers, returns the minimal difference that can be achieved.
  * *
  * For example, given:
  * *
  * A[0] = 3
  * A[1] = 1
  * A[2] = 2
  * A[3] = 4
  * A[4] = 3
  * the function should return 1, as explained above.
  * *
  * Write an efficient algorithm for the following assumptions:
  * *
  * N is an integer within the range [2..100,000];
  * each element of array A is an integer within the range [−1,000..1,000].
  * Copyright 2009–2020 by Codility Limited. All Rights Reserved. Unauthorized copying, publication or disclosure prohibited.
  */
