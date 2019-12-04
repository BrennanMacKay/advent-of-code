package com.github.brennanmackay.aoc.year2018.dayeighteen

import com.github.brennanmackay.aoc.common.Common

object DayEighteen {

  case class Point(x: Int, y: Int) {
    def surrounding: Seq[Point] = {
      Seq(Point(x + 1, y),
          Point(x + 1, y + 1),
          Point(x + 1, y - 1),
          Point(x, y + 1),
          Point(x, y - 1),
          Point(x - 1, y),
          Point(x - 1, y + 1),
          Point(x - 1, y - 1))
    }
  }
  case class Area(field: Array[Array[Char]]) {
    def next: Area = {
      val nextField = Array.fill(field.size, field.head.size)('.')

      for {
        y <- field.indices
        x <- field.head.indices
      } {
        val point = Point(x, y)
        val char = charAt(point) match {
          case Some(char) => {
            char match {
              case '.' => if (charsAround(point, '|') >= 3) '|' else '.'
              case '|' => if (charsAround(point, '#') >= 3) '#' else '|'
              case '#' =>
                if (charsAround(point, '#') >= 1 && charsAround(point, '|') >= 1)
                  '#'
                else '.'
            }
          }
          case None => throw new RuntimeException
        }
        
        nextField(y)(x) = char
      }
      
      Area(nextField)
    }

    def charsAround(point: Point, char: Char): Int = {
      point.surrounding.flatMap(charAt).count(_ == char)
    }
    
    def chars(char: Char): Int = {
      field.flatMap(_.map(c => if(c == char) 1 else 0)).sum
    }

    def withinXBounds(point: Point): Boolean =
      point.x >= 0 && point.x < field.head.size
    def withinYBounds(point: Point): Boolean =
      point.y >= 0 && point.y < field.size

    def charAt(point: Point): Option[Char] = {
      if (withinXBounds(point) && withinYBounds(point)) {
        Some(field(point.y)(point.x))
      } else {
        None
      }
    }

    def view: Unit = {
      println
      field.foreach(row => println(row.mkString))
    }
  }

  def buildArea(name: String): Area = {
    Area(Common.loadData(name).map(_.trim.toCharArray).toArray)
  }
  
  def iterate(area: Area, times: Int, mod: Int): Area = {
    def rec(area: Area, count: Int): Area = {
      if (count % 28 == mod) {
        //area.view
        val trees = area.chars('|')
        val yards = area.chars('#')
        println(s"$count Trees: $trees Yards: $yards wood: ${trees * yards}")
      }

      if (count == times) {
        area
      } else {
        rec(area.next, count + 1)
      }
    }
    
    rec(area, 0)
  }

  def main(args: Array[String]): Unit = {
    val area = buildArea("day-eighteen-input.txt")
    //area.view
    
    val mod = 1000000000 % 28
    val result = iterate(area, 1000, mod)
    val trees = result.chars('|')
    val yards = result.chars('#')

    result.view


    println(mod)
    println(s"Trees: $trees Yards: $yards wood: ${trees * yards}")
    
    
  }

}
