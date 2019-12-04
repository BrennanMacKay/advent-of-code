package com.github.brennanmackay.aoc.year2018.dayten

import com.github.brennanmackay.aoc.common.Common

object DayTen {

  case class Coord(x: Int, y: Int) {
    def move(velocity: Velocity): Coord = {
      Coord(x + velocity.x, y + velocity.y)
    }

    def normalize(bounds: Bounds): Coord = {
      Coord(x - bounds.min.x, y - bounds.min.y)
    }
  }
  case class Velocity(x: Int, y: Int)
  case class Bounds(min: Coord, max: Coord) {
    def size: (Int, Int) = {
      (math.abs(min.x-max.x), math.abs(min.y-max.y))
    }
    def area: Int = {
      val (x, y) = size
      x * y
    }
  }
  case class Light(coord: Coord, velocity: Velocity) {
    def step: Light = {
      Light(coord.move(velocity), velocity)
    }
  }

  case class State(lights: Seq[Light]) {
    def next(): State = {
      State(lights.map(_.step))
    }

    def bounds: Bounds = {
      val minX = lights.minBy(_.coord.x).coord.x
      val maxX = lights.maxBy(_.coord.x).coord.x
      val minY = lights.minBy(_.coord.y).coord.y
      val maxY = lights.maxBy(_.coord.y).coord.y
      Bounds(Coord(minX, minY), Coord(maxX, maxY))
    }

    def view: Unit = {
      val bounding = bounds
      val size = bounding.size

      val array = Array.fill(size._2 + 1, size._1 + 1)('.')

      //println(bounds)
      lights.foreach(light => {
        val norm = light.coord.normalize(bounding)
        //println(s"light: $light normalized: $norm")
        array(norm.y)(norm.x) = '#'
      })

      //println("  " + (0 until size._1 + 1).mkString(" "))
      array.zipWithIndex.foreach(arr => println(arr._1.mkString(" ")))
    }
  }

  def buildLight(str: String): Light = {
    val regex = """^position=<.*?(-?\d+),.*?(-?\d+)> velocity=<.*?(-?\d+),.*?(-?\d+)>$""".r

    str match {
      case regex(x, y, xV, yV) => Light(Coord(x.toInt, y.toInt), Velocity(xV.toInt, yV.toInt))
      case _ => throw new RuntimeException("Bad Data")
    }
  }

  def buildState(strs: Seq[String]): State = {
    State(strs.map(buildLight _))
  }

  def process(state: State): Seq[State] = {
    def rec(xSize: Int, states: Seq[State]): Seq[State] = {
      val next = states.head.next()
      val nextXSize = next.bounds.size._1
      if (nextXSize > xSize) {
        states
      } else {
        rec(nextXSize, next +: states)
      }
    }


    rec(state.bounds.size._1, Seq(state))
  }

  def main(args: Array[String]): Unit = {
    val states = process(buildState(Common.loadData("day-ten-input.txt")))
    states.head.view

    println(states.size)
  }
}
