package com.github.brennanmackay.aoc.dayfive

import common.Common

object DayFive {

  def isOpposite(head: Char, curr: Char): Boolean = {
    head != curr && head.toLower == curr.toLower
  }

  def shouldRemove(test: Char)(other: Char): Boolean = {
    test.toLower == other.toLower
  }

  def consume(poly: Stream[Char], function: (Char) => Boolean): List[Char] = {
    def rec(remaining: Stream[Char], collapsed: List[Char]): List[Char] = {
      if (remaining.isEmpty) {
        collapsed
      } else if (function(remaining.head)) {
        rec(remaining.tail, collapsed)
      } else if (collapsed.isEmpty) {
        rec(remaining.tail, remaining.head :: collapsed)
      } else {
        if(isOpposite(collapsed.head, remaining.head)) {
          rec(remaining.tail, collapsed.tail)
        } else {
          rec(remaining.tail, remaining.head :: collapsed)
        }
      }
    }

    rec(poly, List()).reverse
  }

  def main(args: Array[String]): Unit = {
    val stream = Common.loadData("day-five-input.txt").head.toCharArray.toStream

    val collapsed = consume(stream, c => false)

    println(collapsed.mkString)
    println(collapsed.size)

    val charRange = 'a' to 'z'

    val res = charRange.map(char => {
      (char, consume(stream, shouldRemove(char)).size)
    }).minBy(_._2)

    println(res)
  }
}
