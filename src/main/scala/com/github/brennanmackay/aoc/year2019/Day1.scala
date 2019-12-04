package com.github.brennanmackay.aoc.year2019

import com.github.brennanmackay.aoc.common.Common

object Day1 {

  def main(args: Array[String]): Unit = {

    val data = Common.loadData("2019/1.txt").map(_.toInt)
    val fuel = data.map(calculateFuelP1).sum

    println(fuel)
    
    val fuel2 = data.map(calculateFuelP2).sum
    
    println(fuel2)

  }

  // f = floor(mass / 3) - 2
  def calculateFuelP1(mass: Int): Int = {
    (mass / 3) - 2
  }
  
  def calculateFuelP2(mass: Int): Int = {
    val fuel = calculateFuelP1(mass)
    if (fuel <= 0) {
      0
    } else {
      fuel + calculateFuelP2(fuel)
    }
  }

}
