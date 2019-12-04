package com.github.brennanmackay.aoc.year2018.dayeleven

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object DayEleven {

  case class Grid(grid: Array[Array[Int]]) {
    def view: Unit = {
      grid.zipWithIndex.foreach(arr => println(arr._1.foreach(v => {
        if (v >= 0) print(s"  $v") else print(s" $v")
      })))
    }

    def power(x: Int, y: Int): Int = {
      grid(y - 1)( x - 1)
    }

    def maxPower(): ((Int, Int, Int), Int) = {
      (1 until 50).map(size => {
        println(size)
        val max = maxPower(size)
        println(s"$size: $max")
        (max, size)
      }).maxBy(_._1._1)
    }

    def maxPower(size: Int): (Int, Int, Int) = {

      val results = ListBuffer[(Int, Int, Int)]()
      for {
        y <- 0 until (300-size)
        x <- 0 until (300-size)
      } {
        results += ((grid.slice(y, y+size).map(_.slice(x, x+size).sum).sum, x + 1, y + 1))
      }

      results.maxBy(_._1)
    }
  }

  def buildGrid(serial: Int): Grid = {
    val grid = Array.fill(300,300)(0)

    for {
      y <- 0 until 300
      x <- 0 until 300
    } grid(y)(x) = power(x + 1, y + 1, serial)

    Grid(grid)
  }

  def getDigitFromRight(value: Int, pos: Int): Int = {
    val charArray = value.toString.toCharArray
    if (charArray.size >= 3) {
      charArray.reverse.toVector(2).asDigit
    } else {
      0
    }
  }

  def power(x: Int, y: Int, serial: Int): Int = {
    val rackId = x + 10
    getDigitFromRight(((rackId * y) + serial) * rackId, 3) - 5
  }

  def view(grid: Array[Array[Int]]): Unit = {
    grid.zipWithIndex.foreach(arr => println(arr._1.foreach(v => {
      if (v >= 0) print(s"  $v") else print(s" $v")
    })))
  }

  def main(args: Array[String]): Unit = {

    val grid = buildGrid(1955)

    grid.view

    println(grid.power(217, 196))

    println(grid.maxPower(3))
    println(grid.maxPower())

  }
}
