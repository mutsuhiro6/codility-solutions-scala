package com.github.mutsuhiro6.codility.lessons

/**
  * Created by iwamoto on 2020/10/27
  **/
object CyclicRotation {

  def solution(a: Array[Int], k: Int): Array[Int] = {
    var ans: Array[Int] = a
    for (_ <- 0 until k) {
      ans = rotate(ans)
    }
    ans
  }

  def rotate(a: Array[Int]): Array[Int] = {
    if (a.isEmpty) a
    else {
      val last = Seq(a.last)
      val inits = a.init.toSeq
      (last ++: inits).toArray
    }
  }

}
