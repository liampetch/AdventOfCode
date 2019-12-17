package com.lpetch.adventofcode.twenty19

import scala.annotation.tailrec
import scala.io.Source

object DayThree extends App {
  val WireMap = Source.fromFile("input/DayThree.txt").getLines.toArray.map(current => current.split(','))

  def makeVector(instruction: String, originX: Int, originY: Int): Vector2D = {
    val direction = instruction(0)
    val magnitude = instruction.substring(1).toInt
    direction match {
      case 'U' => new Vector2D(0, magnitude, Point(originX, originY))
      case 'R' => new Vector2D(magnitude, 0, Point(originX, originY))
      case 'D' => new Vector2D(0, -magnitude, Point(originX, originY))
      case 'L' => new Vector2D(magnitude, 0, Point(originX, originY))
      case _ => throw new Exception("Unknown direction instruction")
    }
  }

  @tailrec
  def makeVectorOfVectors(instructions: Array[String], previous: (Int, Int) = (0, 0), vector: Vector[Vector2D] = Vector.empty): Vector[Vector2D] = {
    if (instructions.isEmpty) {
      vector
    } else {
      val instruction = instructions.head
      val (x, y) = previous
      val last = makeVector(instruction, x, y)
      makeVectorOfVectors(instructions.tail, (last.end().x, last.end().y), vector :+ last)
    }
  }

  def findIntersection(v1: Vector2D, v2: Vector2D): Point = {
    // Line AB represented as a1x + b1y = c1
    val a1 = v1.end().y - v1.origin.y
    val b1 = v1.origin.x - v1.end().x
    val c1 = a1*v1.origin.x + b1*v1.origin.y;

    // Line CD represented as a2x + b2y = c2
    val a2 = v2.end().y - v2.origin.y
    val b2 = v2.origin.x - v2.end().x
    val c2 = a2*v2.origin.x+ b2*v2.origin.y

    val determinant = a1*b2 - a2*b1

    if (determinant == 0) {
      Point(Int.MaxValue, Int.MaxValue)
    } else {
      val x = (b2 * c1 - b1 * c2) / determinant
      val y = (a1 * c2 - a2 * c1) / determinant
      if (x < 25 && x > -25 && y < 25 && y > -25) println("Intersection calc" + Point(x,y))
      Point(x, y)
    }
  }

  def manhattanDistance(pointOne: Point, pointTwo: Point): Int = Math.abs(pointTwo.x - pointOne.x) + Math.abs(pointTwo.y - pointOne.y)

  println(findIntersection(Vector2D(10, 0, Point(0, 1)), Vector2D(0,5,Point(1,0))))

  val Wire1 = makeVectorOfVectors(WireMap(0))
  val Wire2 = makeVectorOfVectors(WireMap(1))

  var minManhattanDist = 0

  for (
    vector1 <- Wire1;
    vector2 <- Wire2
  ) {
    val pointOfIntersection = findIntersection(vector1, vector2)
    if (!(pointOfIntersection.x == Int.MaxValue && pointOfIntersection.y == Int.MaxValue)) {
      val dist = manhattanDistance(Point(0, 0), pointOfIntersection)
      if (minManhattanDist == 0 || dist < minManhattanDist) {
        println("---------------------")
        println(pointOfIntersection)
        println("---------------------")
        minManhattanDist = dist
      }
    }
  }

  println("Smallest Manhattan Distance:" + minManhattanDist)
}

case class Vector2D( magnitudeX: Int,  magnitudeY: Int,  origin: Point) {
  def end(): Point = Point(origin.x + magnitudeX, origin.y + magnitudeY)
}

case class Point(x: Int, y: Int)
