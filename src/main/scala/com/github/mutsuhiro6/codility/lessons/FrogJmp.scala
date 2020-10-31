package com.github.mutsuhiro6.codility.lessons

object FrogJmp {

  // 100%
  def solution(x: Int, y: Int, d: Int): Int = {
    var ans: Int = 0
    if (x == y) {
      ans = 0
    }
    else if (y - x <= d) {
      ans = 1
    }
    else {
      if ((y - x) % d == 0) {
        ans = (y - x) / d
      }
      else {
        ans = (y - x) / d + 1
      }
    }
    ans
  }

  /**
    *
    * @param args Input as "(10, 85, 30)".
    */
  def main(args: Array[String]): Unit = {
    val elms = args.head
      .replace("(", "")
      .replace(")", "")
      .replace(" ", "")
      .split(",")
      .map (_.toInt)
    val x = elms.head
    val y = elms(1)
    val d = elms.last
    println(solution(x, y, d))
  }

}

/**
  * A small frog wants to get to the other side of the road. The frog is currently located at position X and wants to get to a position greater than or equal to Y. The small frog always jumps a fixed distance, D.
  * *
  * Count the minimal number of jumps that the small frog must perform to reach its target.
  * *
  * Write a function:
  * *
  * object Solution { def solution(x: Int, y: Int, d: Int): Int }
  * *
  * that, given three integers X, Y and D, returns the minimal number of jumps from position X to a position equal to or greater than Y.
  * *
  * For example, given:
  * *
  * X = 10
  * Y = 85
  * D = 30
  * the function should return 3, because the frog will be positioned as follows:
  * *
  * after the first jump, at position 10 + 30 = 40
  * after the second jump, at position 10 + 30 + 30 = 70
  * after the third jump, at position 10 + 30 + 30 + 30 = 100
  * Write an efficient algorithm for the following assumptions:
  * *
  * X, Y and D are integers within the range [1..1,000,000,000];
  * X ≤ Y.
  */
