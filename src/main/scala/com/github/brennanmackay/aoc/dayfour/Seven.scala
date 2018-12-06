package com.github.brennanmackay.aoc.dayfour

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter

import common.Common

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Seven {

  case class Day(day: LocalDate, minutes: Array[Int])

  case class Guard(id: Int, days: ListBuffer[Day])

  case class Entry(time: LocalDateTime, event: String)

  def setSleepMins(day: Day, start: Int, end: Int): Day = {

    val array = day.minutes.clone()
    for {
      i <- start until end
    } array(i) = 1

    day.copy(minutes = array)
  }

  def getMinute(entry: Entry): Int = {
    entry.time.getMinute
  }

  def processEntries(entries: Seq[Entry]): Seq[Guard] = {
    val startRegex = """Guard #(\d+) begins shift""".r
    val sleepRegex = """falls asleep""".r
    val wakeRegex = """wakes up""".r

    def rec(remainingEntries: Seq[Entry],
            currGuard: Guard,
            currDay: Day,
            guards: mutable.Map[Int, Guard],
            sleepMin: Option[Int]): Map[Int, Guard] = {
      if (remainingEntries.isEmpty) {
        val updatedDay =
          sleepMin.map(min => setSleepMins(currDay, min, 59)).getOrElse(currDay)
        currGuard.days += updatedDay
        guards.put(currGuard.id, currGuard)
        guards.toMap
      } else {
        val entry = remainingEntries.head
        entry.event match {
          case startRegex(id) =>
            // Inclusive sleep minute
            val updatedDay = sleepMin
              .map(min => setSleepMins(currDay, min, 60))
              .getOrElse(currDay)
            currGuard.days += updatedDay
            guards.put(currGuard.id, currGuard)
            val guard =
              guards.getOrElse(id.toInt, Guard(id.toInt, ListBuffer()))
            rec(remainingEntries.tail,
                guard,
                Day(entry.time.toLocalDate, Array.fill(60)(0)),
                guards,
                None)
          case sleepRegex() =>
            rec(remainingEntries.tail,
                currGuard,
                currDay,
                guards,
                Some(getMinute(entry)))
          case wakeRegex() =>
            // Exclusive sleep minute
            val updatedDay = sleepMin
              .map((min => setSleepMins(currDay, min, getMinute(entry))))
              .getOrElse(currDay)
            rec(remainingEntries.tail, currGuard, updatedDay, guards, None)
          case _ =>
            throw new RuntimeException(s"Unknown row: ${entry.event}")
        }
      }
    }

    val result = entries.head.event match {
      case startRegex(id) =>
        rec(entries.tail,
            Guard(id.toInt, ListBuffer()),
            Day(entries.head.time.toLocalDate, Array.fill(60)(0)),
            mutable.Map.empty,
            None)
      case _ => throw new RuntimeException("Bad first entry")
    }

    result.values.toSeq
  }

  def read(): Seq[Entry] = {
    val format = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm")
    val regex = """^\[(\d\d\d\d-\d\d-\d\d \d\d:\d\d)\] (\w.+)$""".r

    val ordering: Ordering[LocalDateTime] = _ compareTo _
    Common
      .loadData("day-four-input.txt")
      .flatMap(line => {
        line match {
          case regex(date, event) =>
            val parsedDate = LocalDateTime.parse(date, format)
            Some(Entry(parsedDate, event))
          case _ => None
        }
      })
      .sortBy(_.time)(ordering)
  }


  def mostSleep(guards: Seq[Guard]): (Guard, Int) = {
    guards
      .map(guard => (guard, guard.days.map(_.minutes.sum).sum))
      .reduceLeft((a, b) => if (a._2 > b._2) a else b)
  }


  def sumArrays(a: Array[Int], b: Array[Int]): Array[Int] = {
    a.zip(b).map(tup => tup._1 + tup._2)
  }

  def mostMinute(guard: Guard): (Int, Int) = {
    val res = guard.days.map(_.minutes).reduce(sumArrays)
    res.zipWithIndex.maxBy(_._1)
  }

  def main(args: Array[String]): Unit = {
    val entries = read
   // entries.foreach(println(_))

    val guards = processEntries(entries)

    //guards.foreach(println(_))


    val most = mostSleep(guards)
    println(s"Most Sleep: ${most._1.id} mins: ${most._2}")
    print("YYYY-MM-DD 0123456789012345678901234567890123456789012345678901234567890")

    most._1.days.foreach(day => {
      print(s"\n${day.day} ")
      day.minutes.foreach(print(_))
    })
    val minute = mostMinute(most._1)
    println(s"\nmost min: ${minute._2} val: ${minute._1}")
    println(most._1.id * minute._2)

    val commonMin = guards.map(guard => (guard, mostMinute(guard))).maxBy(_._2._1)
    println(s"guard: ${commonMin._1.id} min: ${commonMin._2._2}")
    println(commonMin._1.id * commonMin._2._2)
  }
}
