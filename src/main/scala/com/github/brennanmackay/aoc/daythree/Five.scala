package com.github.brennanmackay.aoc.daythree

import common.Common

object Five {

  case class Square(id: Int, x: Int, y: Int, w: Int, h: Int)

  def read(): Seq[Square] = {
    val regex = """^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$""" r

    Common
      .loadData("5-input.txt")
      .flatMap(line => {
        line match {
          case regex(id, x, y, w, h) =>
            Some(Square(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt))
          case _ => None
        }
      })

  }

  def buildSheet(squares: Seq[Square]): Array[Array[Int]] = {
    val sheet = Array.fill(1500, 1500)(0)
    squares.foreach(square => {
      for {
        i <- square.x until square.x + square.w
        j <- square.y until square.y + square.h
      } sheet(i)(j) = sheet(i)(j) + 1
    })

    sheet
  }

  def checkSheet(squares: Seq[Square], sheet: Array[Array[Int]]): Square = {
    squares.flatMap(isAlone(_, sheet)).head
  }

  def isAlone(square: Square, sheet: Array[Array[Int]]): Option[Square] = {
    for {
      i <- square.x until square.x + square.w
      j <- square.y until square.y + square.h
    } if (sheet(i)(j) > 1) return None

    Some(square)
  }

  def main(args: Array[String]): Unit = {
    val squares = read()
    val sheet = buildSheet(squares)

    print(sheet.map(_.mkString).mkString("\n"))
    println()
    println(sheet.map(ar => ar.count(_ > 1)).sum)

    println(checkSheet(squares, sheet))
  }

}
