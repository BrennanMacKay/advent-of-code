package com.github.brennanmackay.aoc.daythirteen

import com.github.brennanmackay.aoc.common.Common
import com.github.brennanmackay.aoc.daythirteen.DayThirteen.Legend.{
  CurveNorth,
  CurveSouth,
  H,
  Junction,
  V
}
import com.github.brennanmackay.aoc.daythirteen.DayThirteen.TurnDirection.{
  Straight,
  TurnLeft,
  TurnRight
}
import enumeratum._
import enumeratum.values.{CharEnum, CharEnumEntry}

import scala.collection.{mutable, SortedMap}

object DayThirteen {

  sealed abstract class Legend(val value: Char) extends CharEnumEntry
  object Legend extends CharEnum[Legend] {
    val values = findValues

    case object H extends Legend('-')
    case object V extends Legend('|')
    case object CurveSouth extends Legend('\\')
    case object CurveNorth extends Legend('/')
    case object Junction extends Legend('+')
    case object Empty extends Legend(' ')
  }

  sealed trait TurnDirection extends EnumEntry {
    def next: TurnDirection
  }

  object TurnDirection extends Enum[TurnDirection] {
    val values = findValues

    case object TurnLeft extends TurnDirection {
      def next: TurnDirection = Straight
    }
    case object Straight extends TurnDirection {
      def next: TurnDirection = TurnRight
    }
    case object TurnRight extends TurnDirection {
      def next: TurnDirection = TurnLeft
    }
  }

  case class Point(x: Int, y: Int) {
    def transform(vector: Vector): Point = {
      Point(x + vector.x, y + vector.y)
    }
  }

  object Point {
    implicit def ordering[A <: Point]: Ordering[A] =
      Ordering.by(p => (p.y, p.x))
  }

  case class Vector(x: Int, y: Int) {
    def transform(turnDirection: TurnDirection): Vector = {
      turnDirection match {
        case TurnLeft  => if (x == 0) Vector(y, x) else Vector(-y, -x)
        case Straight  => Vector(x, y)
        case TurnRight => if (x == 0) Vector(-y, -x) else Vector(y, x)
      }
    }

    def transform(legend: Legend): Vector = {
      legend match {
        case CurveSouth => Vector(y, x)
        case CurveNorth => Vector(-y, -x)
        case _ => throw new RuntimeException
      }
    }

    def toChar: Char = {
      if (x == 0) {
        if (y == 1) 'v' else '^'
      } else {
        if (x == 1) '>' else '<'
      }
    }
  }

  case class Cart(point: Point, vector: Vector, nextTurn: TurnDirection) {
    def next(map: RailMap): Cart = {
      val nextPoint = point.transform(vector)
      val next = map.charAt(nextPoint)
      next match {
        case H => this.copy(point = nextPoint)
        case V => this.copy(point = nextPoint)
        case CurveSouth =>
          this.copy(point = nextPoint, vector = vector.transform(next))
        case CurveNorth =>
          this.copy(point = nextPoint, vector = vector.transform(next))
        case Junction =>
          this.copy(point = nextPoint,
                    vector = vector.transform(nextTurn),
                    nextTurn.next)
        case _ => throw new RuntimeException
      }
    }
  }

  case class State(carts: Map[Point, Cart],  destructionPoints: Seq[Point]) {
//    def next(map: RailMap, itter: Int): State = {
//      val danger = mutable.Set() ++ carts.keys
//      def rec(remaining: Iterable[Cart],  destroyedAt: Seq[Point]): State = {
//        if (remaining.isEmpty) {
//          (State(carts, destroyedAt))
//        } else if (destroyedTick.contains(remaining.head.point)) {
//          rec(remaining.tail, destroyedTick, destroyedAt)
//        } else {
//          val newCart = remaining.head.next(map)
//          carts -= remaining.head.point
//          val cartAtPoint = carts.get(newCart.point)
//          if (cartAtPoint.isDefined) {
//            println(s"Collision! ${newCart.point}, Itter: $itter")
//            //carts -= newCart.point
//            rec(remaining.tail, destroyedTick:+ newCart.point, destroyedAt :+ newCart.point)
//          } else {
//            carts += (newCart.point -> newCart)
//            rec(remaining.tail, destroyedTick, destroyedAt)
//          }
//        }
//      }
//      
//      val newState = rec(carts.values, Seq(), destructionPoints)
//      carts -- newState._2
//      newState._1
//    }
    
    def next(map: RailMap, iter: Int): State = {
      val danger = mutable.Set() ++ carts.keys
      def rec(remaining: Iterable[Cart], updatedCarts: Set[Cart], destroyedTick: Set[Point], destroyedAt: Seq[Point]): (Set[Cart], Seq[Point]) = {
        if (remaining.isEmpty) {
          (updatedCarts, destroyedAt)
        } else if (destroyedTick.contains(remaining.head.point)) {
          rec(remaining.tail, updatedCarts, destroyedTick, destroyedAt)
        } else {
          val newCart = remaining.head.next(map)
          if (danger.contains(newCart.point)) {
            println(s"Collision! ${newCart.point}, Iter: $iter")
            danger -= newCart.point
            rec(remaining.tail, updatedCarts.filter(_.point != newCart.point), destroyedTick + newCart.point, destroyedAt :+ newCart.point)
          } else {
            danger -= remaining.head.point
            danger += newCart.point
            rec(remaining.tail, updatedCarts + newCart, destroyedTick, destroyedAt)
          }
        }
      }
      
      val (newCarts, destroyedAt) = rec(carts.values.toSeq.sortBy(_.point), Set(), Set(), destructionPoints)
      val cartsUpdated = newCarts.map(c => c.point -> c).toMap
      State(cartsUpdated, destroyedAt)
    }
  }

  case class RailMap(map: Array[Array[Char]]) {
    def charAt(point: Point): Legend = {
      Legend.withValue(map(point.y)(point.x))
    }

    def view(carts: Map[Point, Cart]): Unit = {
      map.zipWithIndex.foreach(
        y =>
          println(
            y._1.zipWithIndex
              .map(char => {
                carts
                  .get(Point(char._2, y._2))
                  .map(_.vector.toChar)
                  .getOrElse(char._1)
              })
              .mkString))
    }
  }

  def readMap(fileName: String): (State, RailMap) = {
    val map = Common.loadData(fileName).map(_.toCharArray).toArray
    val carts = mutable.Map[Point, Cart]()
    for {
      y <- 0 until map.size
      x <- 0 until map.head.size
    } {
      map(y)(x) = map(y)(x) match {
        case '^' =>
          carts += (Point(x, y) -> Cart(Point(x, y), Vector(0, -1), TurnLeft))
          '|'
        case '>' =>
          carts += (Point(x, y) -> Cart(Point(x, y), Vector(1, 0), TurnLeft))
          '-'
        case 'v' =>
          carts += (Point(x, y) -> Cart(Point(x, y), Vector(0, 1), TurnLeft))
          '|'
        case '<' =>
          carts += (Point(x, y) -> Cart(Point(x, y), Vector(-1, 0), TurnLeft))
          '-'
        case other => other
      }
    }

    (State(carts.toMap, Seq()), RailMap(map))
  }
  
  def untilCollision(initialState: State, map: RailMap): Point = {
    def rec(state: State, itter: Int): State = {
      if (!state.destructionPoints.isEmpty || itter == 100) {
        state
      } else {
        println(s"iteration: $itter")
        map.view(state.carts)
        rec(state.next(map, itter), itter + 1)
      }
    }
    
    rec(initialState, 0).destructionPoints.head
  }
  
  def untilLastCart(initialState: State, map: RailMap): Seq[State] = {
    def rec(state: State, iter: Int, states: Seq[State]): Seq[State] = {
      if (state.carts.size == 1) {
        states
      } else {
//        println(s"iteration: $iter, ${state.carts.size}")
//        map.view(state.carts)
//        if (itter == 602) {
//          map.view(state.carts)
//        }
        
        val nextState = state.next(map, iter)
        rec(nextState, iter + 1, states :+ nextState)
      }
    }

    rec(initialState, 0, Seq(initialState))
  }

  def main(args: Array[String]): Unit = {
    val (state, map) = readMap("day-thirteen-input.txt")
    
//    val firstPoint = untilCollision(state, map)
//    println(firstPoint)
    
    val states = untilLastCart(state, map)
    println(states.reverse.head.carts.head)
    println(map.view(states.reverse.head.carts))
    
    //states.take(1).foreach(s => map.view(s.carts))
  }
}
