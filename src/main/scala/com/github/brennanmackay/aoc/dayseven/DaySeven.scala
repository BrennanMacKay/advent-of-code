package com.github.brennanmackay.aoc.dayseven

import com.github.brennanmackay.aoc.common.Common

import scala.collection.SortedSet

object DaySeven {

  case class Edge(parent: Vertex, child: Vertex) {
    def nodes: Seq[Vertex] = {
      Seq(parent, child)
    }
  }

  class Vertex(val symbol: Char, val remaining: Int) {
    def canEqual(other: Any): Boolean = other.isInstanceOf[Vertex]

    override def equals(other: Any): Boolean = other match {
      case that: Vertex =>
        (that canEqual this) &&
          symbol == that.symbol
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(symbol)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }

    def tick: Vertex = {
      new Vertex(symbol, remaining - 1)
    }

    override def toString = s"Vertex($symbol, $remaining)"
  }

  object Vertex {

    def fromString(str: String): Vertex = {
      val char = str.toCharArray.head
      new Vertex(char, char.toInt - 4)
    }

    implicit def ordering[A <: Vertex]: Ordering[A] =
      Ordering.by(e => (e.symbol))
  }

  case class Matrix(vertexIndex: Map[Int, Vertex],
                    positionIndex: Map[Vertex, Int],
                    matrix: Array[Array[Boolean]])

  def readEdges(lines: Seq[String]): Seq[Edge] = {
    val regex = """^Step (\w+) .*step (\w+) .*$""".r
    lines.map(_ match {
      case regex(parent, child) =>
        Edge(Vertex.fromString(parent), Vertex.fromString(child))
      case _ => throw new RuntimeException("Bad Input")
    })
  }

  def collectVertices(
      edges: Seq[Edge]): (Map[Int, Vertex], Map[Vertex, Int]) = {
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
       .toMap,
     vertices.zipWithIndex
       .map(tuple => tuple._1 -> tuple._2)
       .toMap)
  }

  def buildAdjMatrix(index: Map[Vertex, Int],
                     edges: Seq[Edge]): Array[Array[Boolean]] = {
    val adjMatrix = Array.fill[Boolean](index.size, index.size)(false)

    edges.foreach(edge => {
      val i = index(edge.parent)
      val j = index(edge.child)
      // backwards? but makes access easier
      adjMatrix(j)(i) = true
    })

    adjMatrix
  }

  def takeCount(matrix: Matrix,
                completed: Set[Vertex],
                count: Int,
                inProgress: Seq[Vertex] = Seq()): Seq[Vertex] = {
    findNext(matrix, completed, inProgress).take(count)
  }

  def findNext(matrix: Matrix, completed: Set[Vertex], inProgress: Seq[Vertex]): Seq[Vertex] = {
    // println(completed)
    def rec(remaining: Seq[Vertex], candidates: Set[Vertex]): Set[Vertex] = {
      if (remaining.isEmpty) {
        candidates
      } else {
        val current = remaining.head
        if (!completed.contains(current) && !inProgress.contains(current) && getIncompleteParents(
              current,
              completed,
              matrix).isEmpty) {
          rec(remaining.tail, candidates + current)
        } else {
          rec(remaining.tail, candidates)
        }
      }
    }

    rec(matrix.vertexIndex.values.toSeq, Set()).toSeq.sorted
  }

  def findOrderSync(matrix: Matrix): Seq[Vertex] = {
    def rec(remaining: Seq[Vertex], order: Seq[Vertex]): Seq[Vertex] = {
      if (remaining.isEmpty) {
        order
      } else {
        rec(remaining.tail, order :+ takeCount(matrix, order.toSet, 1).head)
      }
    }

    rec(matrix.vertexIndex.values.toSeq, Seq())
  }

  def findTimeAsync(matrix: Matrix, workers: Int): Int = {
    def rec(completed: Set[Vertex], tick: Int, inProgress: Seq[Vertex]): Int = {
      if (completed.size == matrix.vertexIndex.size) {
        tick -1
      } else {
        val updated = inProgress.map(vert => vert.tick)
        val finished = updated.filter(_.remaining == 0)
        val completedWithFinished = completed ++ finished
        val updatedMinusComplete = updated.filter(_.remaining != 0)
        val workersAvailable = workers - updatedMinusComplete.size

        val newJobs = takeCount(matrix, completedWithFinished, workersAvailable, updatedMinusComplete)
        val withNewJobs = updatedMinusComplete ++ newJobs

        //Thread.sleep(500)
        println(
          s"$tick workers available: $workersAvailable  - finished in tick: ${finished
            .mkString(",")} - completed: ${completedWithFinished
            .mkString(",")} - pending: ${withNewJobs.mkString(",")}")
        rec(completedWithFinished, tick + 1, withNewJobs)
      }
    }

    rec(Set(), 0, Seq())
  }

  def getIncompleteParents(vertex: Vertex,
                           completed: Set[Vertex],
                           matrix: Matrix): Seq[Vertex] = {
    val res = matrix
      .matrix(matrix.positionIndex(vertex))
      .zipWithIndex
      .map(tuple => (tuple._1, matrix.vertexIndex(tuple._2)))
      .filter(tuple => tuple._1 && !completed.contains(tuple._2))
      .map(_._2)
    res
  }

  def main(args: Array[String]): Unit = {
    val edges = readEdges(Common.loadData("day-seven-input.txt"))
    val index = collectVertices(edges)
    val matrix = Matrix(index._1, index._2, buildAdjMatrix(index._2, edges))

    println(index._1)

//    matrix.matrix.foreach(i => {
//      i.foreach(j => {
//        print(s"${j} | ")
//      })
//      println()
//    })

    val order = findOrderSync(matrix)
    println
    order.foreach(v => print(v.symbol))
    println

    val time = findTimeAsync(matrix, 5)
    println(time)
  }
}
