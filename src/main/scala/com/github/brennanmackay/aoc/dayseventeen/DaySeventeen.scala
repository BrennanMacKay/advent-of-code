package com.github.brennanmackay.aoc.dayseventeen

import com.github.brennanmackay.aoc.common.Common

object DaySeventeen {

  case class Point(x: Int, y: Int) {
    def add(vector: Vector): Point = {
      Point(x + vector.x, y + vector.y)
    }
  }
  case class Vector(x: Int, y: Int)
  case class ClayMap(clayMap: Array[Array[Char]], originSource: Point) {
    def view: Unit = {
      println
      clayMap.foreach(row => println(row.mkString))
    }

    def withinXBounds(point: Point): Boolean =
      point.x >= 0 && point.x < clayMap.head.size
    def withinYBounds(point: Point): Boolean =
      point.y >= 0 && point.y < clayMap.size

    def charAt(point: Point): Option[Char] = {
      if (withinXBounds(point) && withinYBounds(point)) {
        Some(clayMap(point.y)(point.x))
      } else {
        None
      }
    }

    def draw(points: Seq[Point], char: Char): Unit = {
      points.foreach(p => clayMap(p.y)(p.x) = char)
    }
  }

  def expandX(xStart: Int, xEnd: Int, y: Int): Seq[Point] = {
    (xStart until (xEnd + 1)).map(x => Point(x, y))
  }

  def expandY(x: Int, yStart: Int, yEnd: Int): Seq[Point] = {
    (yStart until (yEnd + 1)).map(y => Point(x, y))
  }

  def expand(point1: Point, point2: Point): Seq[Point] = {
    val combined = Seq(point1, point2)
    val xMin = combined.map(_.x).min
    val xMax = combined.map(_.x).max
    val yMin = combined.map(_.y).min
    val yMax = combined.map(_.y).max

    (xMin until (xMax + 1)).flatMap(x => {
      (yMin until (yMax + 1)).map(y => {
        Point(x, y)
      })
    })
  }

  def buildClayCoords(name: String): Seq[Point] = {
    val singleX = """x=(\d+), y=(\d+)..(\d+)""".r
    val singleY = """y=(\d+), x=(\d+)..(\d+)""".r

    Common
      .loadData(name)
      .flatMap(_ match {
        case singleX(x, yStart, yEnd) =>
          expandY(x.toInt, yStart.toInt, yEnd.toInt)
        case singleY(y, xStart, xEnd) =>
          expandX(xStart.toInt, xEnd.toInt, y.toInt)
        case unknown => throw new RuntimeException(unknown)
      })
  }

  def buildClayMap(coords: Seq[Point]): ClayMap = {
    val minX = coords.map(_.x).min
    val maxX = coords.map(_.x).max
    val minY = coords.map(_.y).min
    val maxY = coords.map(_.y).max
    val initial = Array.fill(maxY + 1, maxX - minX + 3)('.')

    coords.foreach(p => initial(p.y)(p.x - minX + 1) = '#')
    val originSource = Point(500 - minX + 1, minY)
    initial(originSource.y)(originSource.x) = '|'

    ClayMap(initial, originSource)
  }

  def processSource(clayMap: ClayMap, source: Point): Seq[Point] = {
    def findBottom(curr: Point): Point = {
      val next = curr.copy(y = curr.y + 1)
      clayMap.charAt(next) match {
        case Some(char) =>
          char match {
            case '.' => findBottom(next)
            case '#' => curr
            case '~' => curr
            case '|' => curr
          }
        case None => curr
      }
    }

    def findBounds(source: Point): (Point, Point) = {
      def findBound(curr: Point, vector: Vector): Point = {
        val next = curr.add(vector)
        clayMap.charAt(next) match {
          case Some(char) =>
            if (isSupported(next)) {
              char match {
                case '.' => findBound(next, vector)
                case '~' => next
                case '|' => findBound(next, vector)
                case '#' => curr
              }
            } else {
              next
            }
          case None => curr
        }
        
      }

      (findBound(source, Vector(-1, 0)), findBound(source, Vector(1, 0)))
    }

    def isSupported(point: Point): Boolean = {
      clayMap.charAt(point.copy(y = point.y + 1)) match {
        case Some(char) => 
          char match {
            case '.' => false
            case '|' => false
            case '~' => true
            case '#' => true
          }
        case None => false
      }
    }
    
    val bottom = findBottom(source)
    val flow = expand(source, bottom)
    clayMap.draw(flow, '|')
    
    if (isSupported(bottom)) {
      val (leftBound, rightBound) = findBounds(bottom)

      val leftSupport = isSupported(leftBound)
      val rightSupport = isSupported(rightBound)
      
      if(leftSupport && rightSupport) {
        clayMap.draw(expand(leftBound, rightBound), '~')
     //   clayMap.view
        processSource(clayMap, bottom.copy(y = bottom.y - 1))
      } else if (leftSupport) {
        clayMap.draw(expand(leftBound, rightBound), '|')
      //  clayMap.view

        Seq(rightBound)
      } else if (rightSupport) {
        clayMap.draw(expand(leftBound, rightBound), '|')
     //   clayMap.view

        Seq(leftBound)
      } else {
        clayMap.draw(expand(leftBound, rightBound), '|')
  //      clayMap.view

        Seq(rightBound, leftBound)
      }
    } else {
      Seq()
    }
  }
  
  def process(clayMap: ClayMap): Unit = {
    def rec(sources: Seq[Point]): Unit = {
      if (!sources.isEmpty) {
        rec(sources.tail ++ processSource(clayMap, sources.head))
      }
    }
    
    rec(Seq(clayMap.originSource))
  }
  
  def main(args: Array[String]): Unit = {
    val coords = buildClayCoords("day-seventeen-input.txt")
    val map = buildClayMap(coords)

    process(map)

    println
    map.view
    
    val result = map.clayMap.flatMap(_.map(_ match {
      //case '|' => 1
      case '~' => 1
      case _ => 0
    })).sum

    println(result)
  }
}
