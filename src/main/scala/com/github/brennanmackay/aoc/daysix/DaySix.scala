package com.github.brennanmackay.aoc.daysix

import com.github.brennanmackay.aoc.daysix.DaySix.max
import common.Common

import scala.collection.mutable

object DaySix {

  case class Coordinate(x: Int, y: Int) {
    def distance(other: Coordinate): Int = {
      math.abs(x - other.x) + math.abs(y - other.y)
    }
  }

  def max(coords: Seq[Coordinate]): Coordinate = {
    def rec(remaining: Seq[Coordinate], maxX: Int, maxY: Int): Coordinate = {
      if (remaining.isEmpty) {
        Coordinate(maxX, maxY)
      } else {
        rec(remaining.tail,
            math.max(maxX, remaining.head.x),
            math.max(maxY, remaining.head.y))
      }
    }

    rec(coords, 0, 0)
  }

  def buildOwnershipBoard(
      max: Coordinate,
      coords: Seq[Coordinate]): Array[Array[Option[Coordinate]]] = {
    val board = Array.ofDim[Option[Coordinate]](max.y + 1, max.x + 1)

    for {
      y <- 0 until max.y + 1
      x <- 0 until max.x + 1
    } {
      val close = closest(Coordinate(x, y), coords)
      board(y)(x) = close
    }

    board
  }

  def buildWithinDistanceBoard(max: Coordinate, coords: Seq[Coordinate], threshold: Int): Array[Array[Int]] = {
    val board = Array.fill[Int](max.y + 1, max.x + 1)(0)

    for {
      y <- 0 until max.y + 1
      x <- 0 until max.x + 1
    } {
      board(y)(x) = if(totalDistance(Coordinate(x, y), coords) < threshold) 1 else 0
    }

    board
  }

  def totalDistance(currentCoord: Coordinate, otherCoords: Seq[Coordinate]): Int = {
    def rec(remaining: Seq[Coordinate], total: Int): Int = {
      if(remaining.isEmpty) {
        total
      } else {
        rec(remaining.tail, currentCoord.distance(remaining.head) + total)
      }
    }

    rec(otherCoords, 0)
  }

  def closest(currentCoord: Coordinate,
              otherCoords: Seq[Coordinate]): Option[Coordinate] = {
    def rec(remaining: Seq[Coordinate],
            distances: Seq[(Int, Coordinate)],
            min: Int): (Int, Seq[(Int, Coordinate)]) = {
      if (remaining.isEmpty) {
        (min, distances)
      } else {
        val distance = (currentCoord.distance(remaining.head), remaining.head)
        rec(remaining.tail, distances :+ distance, math.min(distance._1, min))
      }
    }

    val result = rec(otherCoords, Seq(), Int.MaxValue)
    val maxDistance = result._1
    val distances = result._2

    val coords = distances.filter(_._1 == maxDistance)
    if (coords.size == 1) {
      Some(coords.head._2)
    } else {
      None
    }
  }

  def onEdge(coord: Coordinate, max: Coordinate): Boolean = {
    coord.x == 0 || coord.y == 0 || coord.x == max.x || coord.y == max.y
  }

  def largestInfluence(board: Array[Array[Option[Coordinate]]],
                       max: Coordinate,
                       coords: Seq[Coordinate]): (Coordinate, Int) = {
    var count = Map[Coordinate, Int]()
    coords.foreach(coord => count += (coord -> 0))

    for {
      y <- 0 until max.y + 1
      x <- 0 until max.x + 1
    } {
      board(y)(x).foreach(closest => {
        if (onEdge(Coordinate(x, y), max)) {
          count -= closest
        } else {
          count.get(closest).foreach(value => count += (closest -> (value + 1)))
        }
      })
    }

    count.maxBy(_._2)
  }

  def main(args: Array[String]): Unit = {
    val data = Common
      .loadData("day-six-input.txt")
      .map(row => {
        val split = row.split(",").map(_.replace(" ", "").toInt)
        Coordinate(split(0), split(1))
      })

    val maxCoord = max(data)
    println(maxCoord)
//    val board = buildOwnershipBoard(maxCoord, data)
//    board.foreach(x => {
//      x.foreach(y => {
//        val str = y.map(c => s"${c.x}, ${c.y} | ").getOrElse("., . | ")
//        print(str)
//      })
//      println()
//    })

//    println(largestInfluence(board, maxCoord, data))

    val distBoard = buildWithinDistanceBoard(maxCoord, data, 10000)

    distBoard.foreach(x => {
      x.foreach(y => {
        //val str = y.map(c => s"${c.x}, ${c.y} | ").getOrElse("., . | ")
        print(s" $y ")
      })
      println()
    })


    println(distBoard.map(_.sum).sum)
  }
}
