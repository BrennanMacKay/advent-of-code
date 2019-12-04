package com.github.brennanmackay.aoc.year2018.daynine

import java.time.LocalDateTime

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object DayNine {

  class Circle(circle: ArrayBuffer[Int], var current: Int) {
    def nonScoringAdd(num: Int): Circle = {
      val calculated = (current + 2) % circle.size
      val idx = if (calculated == 0) circle.size else calculated
      circle.insert(idx, num)
      //val newVect = (circle.take(idx) :+ num) ++ circle.takeRight(
      //  circle.size - idx)
      current = idx
      //Circle(circle, idx)
      this
    }

    def scoringAdd(num: Int): (Int, Circle) = {
      val calculated = (current - 7) % circle.size
      val idx = if (calculated < 0) circle.size + calculated else calculated
      val score = num + circle(idx)
      circle.remove(idx)
      current = idx % circle.size
      (score, this)
    }

    override def toString: String = {
      s"Current: $current, Circle: ${circle.mkString(", ")}"
    }
  }

  object Circle {
    def init(): Circle = {
      new Circle(ArrayBuffer(0), 0)
    }
  }

  class State(val circle: Circle,
              var remaining: Seq[Int],
              val scores: Array[Long],
              var turn: Int)

  def play(numPlayers: Int, numMarbles: Int): State = {
    @tailrec
    def turn(state: State): State = {
      if (state.remaining.size % 10000 == 0) {
        println(s"${LocalDateTime.now} remaining:  ${state.remaining.size}")
      }

      //println(state.circle)
      if (state.remaining.isEmpty) {
        state
      } else {
        val marble = state.remaining.head
        if (marble % 23 != 0) {
          state.circle.nonScoringAdd(marble)
          state.remaining = state.remaining.tail
          state.turn = (state.turn + 1) % numPlayers
          turn(state)
        } else {
          val (score, circle) = state.circle.scoringAdd(marble)
          state.scores(state.turn) = state.scores(state.turn) + score
          state.remaining = state.remaining.tail
          state.turn = (state.turn + 1) % numPlayers
          turn(state)
        }
      }
    }

    turn(
      new State(Circle.init(),
            (1 until numMarbles + 1).toSeq,
            Array.fill(numPlayers)(0),
            0))
  }

  def main(args: Array[String]): Unit = {
    //println(play(9, 25).scores.max)


    println(
      "10 players; last marble is worth 1618 points: high score is 8317 " + play(
        10,
        1618).scores.max)
    println(
      "13 players; last marble is worth 7999 points: high score is 146373 " + play(
        13,
        7999).scores.max)
    println(
      "17 players; last marble is worth 1104 points: high score is 2764 " + play(
        17,
        1104).scores.max)
    println(
      "21 players; last marble is worth 6111 points: high score is 54718 " + play(
        21,
        6111).scores.max)
    println(
      "30 players; last marble is worth 5807 points: high score is 37305 " + play(
        30,
        5807).scores.max)

    println(
      "458 players; last marble is worth 72019 points " + play(458, 7201900).scores.max)
  }
}
