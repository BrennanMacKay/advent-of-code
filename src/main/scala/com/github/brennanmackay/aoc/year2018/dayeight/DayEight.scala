package com.github.brennanmackay.aoc.year2018.dayeight

import com.github.brennanmackay.aoc.common.Common

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object DayEight {


  case class Node(id: Int, children: Vector[Node], metadata: Array[Int]) {
    def collect(): Seq[Node] = {
      this +: children.flatMap(_.collect())
    }

    def value(): Int = {
      if (children.isEmpty) {
        metadata.sum
      } else {
        val max = children.size
        metadata.map(idx => {
          if (idx == 0 || idx > max) {
            0
          } else {
            children(idx-1).value()
          }
        }).sum
      }
    }
  }


  var id = 1

  def process(data: mutable.Stack[Int]): Seq[Node] = {

    def rec: Node = {
      //println(data)
      val childCount = data.pop()
      val metaCount = data.pop()

      val thisId = id
      id = id + 1
      val children = if (childCount != 0) {
         (0 until childCount).map(_ => {
          rec
        }).toVector
      } else {
        Vector()
      }

      val metaData = (0 until metaCount).map(_ => data.pop()).toArray

      //println(s"id: $thisId | children: ${children.mkString(",")} | meta: ${metaData.mkString(",")}")
      Node(thisId, children, metaData)
    }

    rec.collect()
  }

  def read(file: String): mutable.Stack[Int] = {
    mutable.Stack.empty ++ Common.loadData(file).head.split(" ").map(_.toInt).toSeq
  }

  def main(args: Array[String]): Unit = {
    val data = read("day-eight-input.txt")
    val nodes = process(data)

    println(nodes.flatMap(_.metadata).sum)
    println(nodes.head.value())
  }
}
