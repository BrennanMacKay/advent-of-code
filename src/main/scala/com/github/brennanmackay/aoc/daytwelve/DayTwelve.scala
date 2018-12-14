package com.github.brennanmackay.aoc.daytwelve

import com.github.brennanmackay.aoc.common.Common

object DayTwelve {

  case class Rules(rules: Set[String]) {
    def isNextGen(test: String): Int = {
      val result = rules.contains(test)
      //println(s"test: $test, $result")
      if(rules.contains(test)) 1 else 0
    }
  }

  case class State(pots: Seq[Int], zeroIdx: Int) {
    def next(rules: Rules): State = {

      def rec(rem: Seq[Int], newState: Seq[Int]): Seq[Int] = {
        if (rem.isEmpty) {
          //newState.takeWhile(_ == 0)
          newState ++ Seq(0,0,0)
        } else {
          val entries = rem.take(5)
          if (entries.size == 5) {
            rec(rem.tail, newState :+ rules.isNextGen(entries.mkString))
          } else {
            rec(rem.tail, newState)
          }
        }
      }

      //println(s"${pots.mkString}")
      //val firstPotIdx = pots.takeWhile(_ == 0).size
      //val paddedPots = if (firstPotIdx < 5) List.fill(4 - firstPotIdx)(0) ++: pots else pots
      //println(s"${paddedPots.mkString}")
      val res = State(rec(Seq(0,0,0) ++: pots, Seq()), zeroIdx + 1)
      //println(s"${res.pots.mkString}")
      res
    }
  }

  def process(initialState: State, rules: Rules, generations: Int): Seq[State] = {
    def rec(history: Seq[State], currentGen: Int): Seq[State] = {
      //println(s"$currentGen ${history.head.zeroIdx}: ${history.head.pots.mkString}")
      if (currentGen == generations) {
        history
      } else {
        rec(history.head.next(rules) +: history, currentGen + 1)
      }
    }

    rec(Seq(initialState), 0)
  }

  def main(args: Array[String]): Unit = {
    val stateRegex = """initial state: (.*)""".r
    val positiveRules = """^(.*?) => #$""".r

    val data = Common
      .loadData("day-twelve-input.txt")
      .flatMap(_ match {
        case stateRegex(state)   => Some(state)
        case positiveRules(rule) => Some(rule)
        case _ => None
      }).map(_.replace('.', '0').replace('#', '1'))


//    println(data.head)
    val initialState = State((Seq(0,0,0,0) ++: data.head.toCharArray.map(s => s.asDigit)) ++ Seq(0,0,0,0), 4)
    val strRules = data.tail.sorted
//    println(strRules.toArray.mkString(","))
    val rules = Rules(strRules.toSet)

//    println(rules.rules)

    val result = process(initialState, rules, 1000)
//    println(result.head.pots.mkString(" "))
//    println(result.head.pots.sum)
    println(result.head.pots.zipWithIndex.filter(_._1 == 1).map(t => {
      val res = t._2 - result.head.zeroIdx
      //println(res)
      res
    }).sum)

    result.reverse.zipWithIndex.foreach(state => {
      val sum = state._1.pots.zipWithIndex.filter(_._1 == 1).map(t => {
        val res = t._2 - state._1.zeroIdx
        res
      }).sum

      println(s"gen: ${state._2}, $sum")
    })
  }
}
