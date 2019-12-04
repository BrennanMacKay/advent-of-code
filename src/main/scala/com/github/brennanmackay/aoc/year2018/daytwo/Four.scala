package com.github.brennanmackay.aoc.year2018.daytwo

import com.github.brennanmackay.aoc.common.Common

object Four {

  def exactlyOneDiff(one: String, two: String): Boolean = {
    one.toCharArray
    two.toCharArray

    val zipped = one.zip(two)

    zipped.map(t => if(t._1 != t._2) 1 else 0).sum == 1
  }

  def process(codes: Seq[String],
              all: Seq[String]): Option[(String, String)] = {

    def inner(compare: Seq[String]): Option[(String, String)] = {

      if (compare.isEmpty) {
        None
      } else if (exactlyOneDiff(codes.head, compare.head)) {
        Some(codes.head, compare.head)
      } else {
        inner(compare.tail)
      }
    }

    if (codes.isEmpty) {
      None
    } else {
      val maybeResult = inner(all)
      if (maybeResult.isDefined) {
        maybeResult
      } else {
        process(codes.tail, all)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val codes = Common.loadData("3-input.txt")

    val result = process(codes, codes)

    println(result)

    //cvqlbidheyujgtrswxmckqnap
    //cvqlbidheyujgtrswxmckqnap
  }
}
