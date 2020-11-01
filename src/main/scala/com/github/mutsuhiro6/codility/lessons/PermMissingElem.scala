package com.github.mutsuhiro6.codility.lessons

object PermMissingElem {

  /**
    *
    * @param args Input like Array("(1,3,2,5)").
    */
  def main(args: Array[String]): Unit = {
    val elms = args.head
      .replace("(", "")
      .replace(")", "")
      .replace(" ", "")
      .split(",")
      .map(_.toInt)
    println(solution(elms))
  }

  def solution(a: Array[Int]): Int = {
    val n = a.length
    var diff = 0
    var max = 0
    for (i <- 1 to n) {
      val e = a(i - 1)
      diff += e - i
      if (e > max) max = e
    }
    if (diff == 0) max + 1
    else max - diff
  }
}

/**
  * An array A consisting of N different integers is given. The array contains integers in the range [1..(N + 1)], which means that exactly one element is missing.
  * *
  * Your goal is to find that missing element.
  * *
  * Write a function:
  * *
  * object Solution { def solution(a: Array[Int]): Int }
  * *
  * that, given an array A, returns the value of the missing element.
  * *
  * For example, given array A such that:
  * *
  * A[0] = 2
  * A[1] = 3
  * A[2] = 1
  * A[3] = 5
  * the function should return 4, as it is the missing element.
  * *
  * Write an efficient algorithm for the following assumptions:
  * *
  * N is an integer within the range [0..100,000];
  * the elements of A are all distinct;
  * each element of array A is an integer within the range [1..(N + 1)].
  * Copyright 2009–2020 by Codility Limited. All Rights Reserved. Unauthorized copying, publication or disclosure prohibited.
  **/