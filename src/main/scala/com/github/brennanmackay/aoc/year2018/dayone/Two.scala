package com.github.brennanmackay.aoc.year2018.dayone

import com.github.brennanmackay.aoc.year2018.dayone.One.loadData

object Two {

  def search(freq: Seq[Int]): Int = {

    def state(subFreq: Seq[Int], sum: Int, seen: Set[Int]): Int = {
      if (subFreq.isEmpty) {
        state(freq, sum, seen)
      } else {
        val newSum = sum + subFreq.head
        if (seen.contains(newSum)) {
          newSum
        } else {
          state(subFreq.tail, newSum, seen + newSum)
        }
      }
    }

    state(freq, 0, Set())
  }

  def main(args: Array[String]): Unit = {
    val res = One.loadData()
    print(search(res))
  }
}
