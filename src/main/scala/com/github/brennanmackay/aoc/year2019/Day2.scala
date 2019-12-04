package com.github.brennanmackay.aoc.year2019

import com.github.brennanmackay.aoc.common.Common

object Day2 {

  // 1 add
  // 2 multiply
  // 99 stop

  def main(args: Array[String]): Unit = {
    println(run(toArray("1,9,10,3,2,3,11,0,99,30,40,50")).mkString(","))
    println(run(toArray("1,0,0,0,99")).mkString(","))
    println(run(toArray("2,3,0,3,99")).mkString(","))
    println(run(toArray("2,4,4,5,99,0")).mkString(","))
    println(run(toArray("1,1,1,4,99,5,6,0,99")).mkString(","))

    val instructions = toArray(Common.loadData("2019/2.txt").head)
    println(instructions.mkString(","))

    // replace pos 1 with `12` and pos 2 with `2`
    instructions.update(1, 12)
    instructions.update(2, 2)

    println(instructions.mkString(","))

    val result = run(instructions)

    println(result.mkString(","))

    val target = 19690720
    
    val findRes = find(instructions, target)
    
    println(findRes)
    println(100 * findRes._1 + findRes._2)
  }

  def find(instructions: Array[Int], target: Int): (Int, Int) = {
    def rec(noun: Int, verb: Int): (Int, Int) = {
      run(instructions, Some(noun), Some(verb))(0) match {
        case v if v == target => (noun, verb)
        case v if v > target => rec(0, verb + 1)
        case v if v < target => rec(noun + 1, verb)
      }
    }
    
    rec(0, 0)
  }

  def run(instructions: Array[Int],
          noun: Option[Int] = None,
          verb: Option[Int] = None): Array[Int] = {
    // mutates state :(
    def step(state: Array[Int], currStep: Int): Array[Int] = {

      val insIndex = currStep * 4
      state(insIndex) match {
        case 1 =>
          val result = state(state(insIndex + 1)) + state(state(insIndex + 2))
          state.update(state(insIndex + 3), result)
          step(state, currStep + 1)
        case 2 =>
          val result = state(state(insIndex + 1)) * state(state(insIndex + 2))
          state.update(state(insIndex + 3), result)
          step(state, currStep + 1)
        case 99 =>
          state
      }

    }
    val ownedState = instructions.clone()
    ownedState.update(1, noun.getOrElse(ownedState(1)))
    ownedState.update(2, verb.getOrElse(ownedState(2)))

    step(ownedState, 0)
  }

  def toArray(instructions: String): Array[Int] = {
    instructions.split(",").map(_.toInt)
  }

}
