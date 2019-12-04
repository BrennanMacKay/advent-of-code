package com.github.brennanmackay.aoc.year2018.dayone

import scala.io.Source

object One {
  def loadData(): Seq[Int] = {
    Source
      .fromResource("1-input.txt")
      .getLines()
      .map(_.replace("+","").toInt)
      .toSeq
  }

  def main(args: Array[String]): Unit = {
    val res = loadData().sum
    print(res)
  }

}
