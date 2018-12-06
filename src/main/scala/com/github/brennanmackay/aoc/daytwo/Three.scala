package com.github.brennanmackay.aoc.daytwo

import common.Common

import scala.collection.mutable

object Three {


  def isTwo(map: Map[Char, Int]): Boolean = {
    map.values.toSet.contains(2)
  }

  def isThree(map: Map[Char, Int]): Boolean = {
    map.values.toSet.contains(3)
  }

  def letterCounts(input: String): Map[Char, Int] = {
    val map = mutable.HashMap.empty[Char, Int]
    input.toCharArray.foreach(c => {
      map.update(c, map.getOrElseUpdate(c, 0) + 1)
    })

    map.toMap
  }

  def counts(codes: Seq[String]): (Int, Int) = {

    def rec(codes: Seq[String], count: (Int, Int)): (Int, Int) = {
      if (codes.isEmpty) {
        count
      } else {
        val counts: Map[Char, Int] = letterCounts(codes.head)
        val two = if (isTwo(counts)) 1 else 0
        val three = if (isThree(counts)) 1 else 0
        rec(codes.tail, (count._1 + two, count._2 + three))
      }
    }

    rec(codes, (0, 0))
  }


  def main(args: Array[String]): Unit = {
    val codes = Common.loadData("3-input.txt")

    val count = counts(codes)

    println(count)

    println(count._1 * count._2)
  }
}
