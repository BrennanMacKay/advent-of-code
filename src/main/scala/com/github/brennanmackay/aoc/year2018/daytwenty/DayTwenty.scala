package com.github.brennanmackay.aoc.year2018.daytwenty

import com.github.brennanmackay.aoc.common.Common

import scala.collection.immutable.Stack
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object DayTwenty {

  val directions = Set('N', 'S', 'E', 'W')

  case class Point(x: Int, y: Int) {
    def toDirection(dir: Char): Point = {
      dir match {
        case 'N' => this.copy(y = y - 2)
        case 'S' => this.copy(y = y + 2)
        case 'E' => this.copy(x = x + 2)
        case 'W' => this.copy(x = x - 2)
        case bad => throw new RuntimeException(bad.toString)
      }
    }
  }
  
  class PathSegment(directions: Seq[Char], children: Seq[PathSegment]) {
    
  }

  case class Maze(points: mutable.Set[Point],
                  doors: mutable.Map[Point, mutable.Set[Point]])

  def ingestMaze(name: String): Maze = {
    val maze = Maze(mutable.Set(Point(0, 0)),
                    mutable.Map(Point(0, 0) -> mutable.Set.empty))
//    val preBranch = mutable.Stack[Point]()
    var currentPath = ListBuffer[Char]()
//    var current = Point(0, 0)
    Common.loadData(name).head.toCharArray.foreach {
      case dir if directions.contains(dir) =>
//        val next = current.toDirection(dir)
//        maze.points.add(next)
//        maze.doors(current).add(next)
//        maze.doors.getOrElseUpdate(next, mutable.Set.empty).add(current)
//        current = next
        currentPath :+ dir
      case '('                             =>
        
      case ')'                             =>
        
      case '|'                             =>
    }
    
    maze
  }

  def main(args: Array[String]): Unit = {}
}
