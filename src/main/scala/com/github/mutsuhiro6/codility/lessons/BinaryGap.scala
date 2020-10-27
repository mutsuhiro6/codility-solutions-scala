package com.github.mutsuhiro6.codility.lessons

/**
  * Created by iwamoto on 2020/10/27
  **/
object BinaryGap {

  def solution(n: Int): Int = {
    val binary = n.toBinaryString
    var cntTmp = 0
    var cntMax = 0
    var findOne = false
    binary.reverse.foreach {
      elm =>
        if (elm == '0' && findOne) cntTmp += 1
        if (elm == '1') {
          if (cntMax < cntTmp) cntMax = cntTmp
          cntTmp = 0
          findOne = true
        }
    }
    cntMax
  }
}
