package com.lpetch.adventofcode.twenty19

import scala.io.Source

object DayThreeMk2 extends App {
  val WireMap = Source.fromFile("input/DayThree.txt").getLines.toArray

  def manhattanDistance(pointOne: Point, pointTwo: Point): Int =
    Math.abs(pointTwo.x - pointOne.x) + Math.abs(pointTwo.y - pointOne.y)

  def wireStringToPoints(wire: String): List[Point] = {
    wire.split(",").foldLeft(List.empty[Point]) { (pointList, currentInstruction) =>
      val lastPoint = if (pointList.isEmpty) {
        Point(0,0)
      } else {
        pointList.last
      }

      val direction = currentInstruction(0)
      val distance = currentInstruction.substring(1).toInt

      val linePoints = direction match {
        case 'U' => (1 to distance.toInt).map(y => lastPoint.copy(y = lastPoint.y + y))
        case 'R' => (1 to distance.toInt).map(x => lastPoint.copy(x = lastPoint.x + x))
        case 'D' => (1 to distance.toInt).map(y => lastPoint.copy(y = lastPoint.y - y))
        case 'L' => (1 to distance.toInt).map(x => lastPoint.copy(x = lastPoint.x - x))
        case _ => throw new Exception("Unknown direction instruction")
      }
      pointList ++ linePoints
    }
  }

  def distanceToPoint(Wire: List[Point], destination: Point): Int = {
    Wire.indexOf(destination) + 1
  }


  val Wire1 = wireStringToPoints(WireMap(0))
  val Wire2 = wireStringToPoints(WireMap(1))

  val intersectingPoints = Wire1.intersect(Wire2)

  val lowestTotalWireDistance = intersectingPoints.foldLeft(Int.MaxValue) { (lowestTotalWireDistance, currentIntersect) =>
    val length = distanceToPoint(Wire1, currentIntersect) + distanceToPoint(Wire2, currentIntersect)
    if (length < lowestTotalWireDistance && length != 0) {
      length
    } else {
      lowestTotalWireDistance
    }
  }

  println("Lowest distance:" + lowestTotalWireDistance)
}

case class Point(x: Int, y: Int)
