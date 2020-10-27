package com.github.mutsuhiro6.codility.lessons

/**
  * Created by iwamoto on 2020/10/27
  **/
object OddOccurrencesInArray {

  def solution(a: Array[Int]): Int = {
    val candi = scala.collection.mutable.ListBuffer.empty[Int]
    candi += a.head
    for (i <- a.tail) {
     if (candi.contains(i)) candi -= i
     else candi += i
    }
    candi.head
  }
}
