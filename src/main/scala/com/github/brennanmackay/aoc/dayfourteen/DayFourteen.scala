package com.github.brennanmackay.aoc.dayfourteen

object DayFourteen {

  case class Elf(idx: Int, score: Int) {
    def move(size: Int, scoreboard: Vector[Int]): Elf = {
      val newIdx = (idx + 1 + score) % size
      Elf(newIdx, scoreboard(newIdx))
    }
  }

  case class Scoreboard(e1: Elf, e2: Elf, scores: Vector[Int]) {
    def combine: Scoreboard = {
      val combined = e1.score + e2.score
      val newScores = scores ++ combined.toString.toCharArray.map(_.asDigit)
      Scoreboard(e1.move(newScores.size, newScores),
                 e2.move(newScores.size, newScores),
                 newScores)
    }

    def view: Unit = {
      val board = scores.zipWithIndex
        .map(score => {
          if (score._2 == e1.idx) {
            s"(${score._1})"
          } else if (score._2 == e2.idx) {
            s"[${score._1}]"
          } else {
            s" ${score._1} "
          }
        })
        .mkString
      println(board)
    }
  }

  def combineUntil(scoreboard: Scoreboard, recipiesCount: Int): Scoreboard = {
    def rec(updatedBoard: Scoreboard): Scoreboard = {
      //updatedBoard.view
      if (updatedBoard.scores.size > recipiesCount) {
        updatedBoard
      } else {
        rec(updatedBoard.combine)
      }
    }

    rec(scoreboard)
  }

  def combineUntilFound(scoreboard: Scoreboard,
                        input: Vector[Int]): Scoreboard = {
    def rec(updatedBoard: Scoreboard): Scoreboard = {

      def containsSlice: Boolean = {
        -1 != updatedBoard.scores
          .indexOfSlice(input, updatedBoard.scores.size - input.size - 2)
      }

      if (containsSlice) {
        updatedBoard
      } else {
        rec(updatedBoard.combine)
      }
    }

    rec(scoreboard)
  }

  def main(args: Array[String]): Unit = {
    val initial = Scoreboard(Elf(0, 3), Elf(1, 7), Vector(3, 7))

//    val improveAfter = 409551
//    val result = combineUntil(initial, improveAfter + 10)
//    println(result.scores.slice(improveAfter, improveAfter + 10).mkString)

    val finding = "409551"
    val findingSlice = finding.toCharArray.map(_.asDigit).toVector
    val found = combineUntilFound(initial, findingSlice)

    val sliceIdx = found.scores.indexOfSlice(findingSlice)
    println(sliceIdx)
  }

}
