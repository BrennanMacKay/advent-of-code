package com.github.brennanmackay.aoc.dayseven

import common.Common

import scala.collection.SortedSet

object DaySeven {

  case class Edge(parent: Vertex, child: Vertex) {
    def nodes: Seq[Vertex] = {
      Seq(parent, child)
    }
  }

  case class Vertex(symbol: String)

  object  Vertex {

    implicit def ordering[A <: Vertex]: Ordering[A] =
      Ordering.by(e => (e.symbol))
  }


  case class Matrix(vertexIndex: Map[Int, Vertex], positionIndex: Map[Vertex, Int], matrix: Array[Array[Boolean]])

  def readEdges(lines: Seq[String]): Seq[Edge] = {
    val regex = """^Step (\w+) .*step (\w+) .*$""".r
    lines.map(_ match {
      case regex(parent, child) => Edge(Vertex(parent), Vertex(child))
      case _                    => throw new RuntimeException("Bad Input")
    })
  }

  def collectVertices(edges: Seq[Edge]): (Map[Int, Vertex], Map[Vertex, Int]) = {
    def rec(remaining: Seq[Edge],
            vertices: SortedSet[Vertex]): SortedSet[Vertex] = {
      if (remaining.isEmpty) {
        vertices
      } else {
        rec(remaining.tail, vertices ++ remaining.head.nodes)
      }
    }

    val vertices = rec(edges, SortedSet[Vertex]())

    (vertices.zipWithIndex
      .map(tuple => tuple._2 -> tuple._1)
      .toMap, vertices.zipWithIndex
      .map(tuple => tuple._1 -> tuple._2)
      .toMap)
  }

  def buildAdjMatrix(index: Map[Vertex, Int], edges: Seq[Edge]): Array[Array[Boolean]] = {
    val adjMatrix = Array.fill[Boolean](index.size, index.size)(false)

    edges.foreach(edge => {
      val i = index(edge.parent)
      val j = index(edge.child)
      adjMatrix(i)(j) = true
    })

    adjMatrix
  }

  def findNext(matrix: Matrix, completed: Set[Vertex]): Vertex = {
    def rec(remaining: Seq[Vertex], candidates: Set[Vertex]): Set[Vertex] = {
      if (remaining.isEmpty) {
        candidates
      } else {
        remaining.head
      }
    }
  }

  def getIncompleteParents(vertex: Vertex, completed: Set[Vertex], matrix: Matrix): Seq[Vertex] = {
    matrix.
  }


  def main(args: Array[String]): Unit = {
    val edges = readEdges(Common.loadData("day-seven-test.txt"))
    val index = collectVertices(edges)
    val matrix = Matrix(index._1, index._2, buildAdjMatrix(index._2, edges))


    println(index._1)


    matrix.matrix.foreach(i => {
      i.foreach(j => {
        print(s"${j} | ")
      })
      println()
    })

  }
}
