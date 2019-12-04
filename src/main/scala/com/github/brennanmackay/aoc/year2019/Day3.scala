package com.github.brennanmackay.aoc.year2019

import com.github.brennanmackay.aoc.common.Common

object Day3 {
  case class Point(x: Int, y: Int, step: Int) {
    def dist(other: Point): Int = {
      Math.abs(x - other.x) + Math.abs(y - other.y)
    }
  }

  case class Instruction(dir: String, dist: Int)

  def addPoints(state: List[Point],
                curr: Point,
                instruction: Instruction): List[Point] = {
    def nextPoint: Point = {
      instruction.dir match {
        case "U" => curr.copy(y = curr.y + 1, step = curr.step + 1)
        case "D" => curr.copy(y = curr.y - 1, step = curr.step + 1)
        case "L" => curr.copy(x = curr.x - 1, step = curr.step + 1)
        case "R" => curr.copy(x = curr.x + 1, step = curr.step + 1)
      }
    }

    if (instruction.dist <= 0) {
      state
    } else {
      val next = nextPoint
      addPoints(next +: state,
                next,
                instruction.copy(dist = instruction.dist - 1))
    }
  }

  def addInstructions(instructions: List[Instruction]): List[Point] = {
    def rec(instructions: List[Instruction],
            points: List[Point]): List[Point] = {
      instructions match {
        case Nil          => points
        case head :: tail => rec(tail, addPoints(points, points.head, head))
      }
    }

    rec(instructions, List(Point(0, 0, 0))).reverse
  }

  def getInstruction(inst: String): Instruction = {
    val regex = """([UDLR])(\d+)""".r

    inst match {
      case regex(dir, dist) => Instruction(dir, dist.toInt)
      case unknown          => throw new RuntimeException(unknown)
    }
  }

  def getPoints(ins: String): List[Point] = {
    addInstructions(ins.split(",").map(getInstruction).toList)
  }

  def intersections(a: List[Point], b: List[Point]): List[Point] = {
    a
      .map(p => (p.x, p.y))
      .intersect(b.map(p => (p.x, p.y)))
      .filter(_ != (0, 0))
      .map(p => Point(p._1, p._2, 0))
  }

  def minDistance(a: List[Point], b: List[Point]): Option[Int] = {
    val origin = Point(0, 0, 0)
    intersections(a, b).map(p => p.dist(origin)).sorted.headOption
  }

  def firstCross(a: List[Point], b: List[Point]): Option[Int] = {
    val inter = intersections(a, b)
    inter.map(int => {
      val aStep = a.find(a => int.x == a.x && int.y == a.y).get.step
      val bStep = b.find(b => int.x == b.x && int.y == b.y).get.step
      val res = (int, aStep + bStep)
      println(res)
      res
    }).sortBy(_._2).headOption.map(_._2)
  }

  def main(args: Array[String]): Unit = {
    val t1 = getPoints("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
    val t2 = getPoints("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

    println(firstCross(t1, t2))

    val data =
      Common
        .loadData("2019/3.txt")
        .map(getPoints)

    println(firstCross(data.head, data(1)))

  }
}
