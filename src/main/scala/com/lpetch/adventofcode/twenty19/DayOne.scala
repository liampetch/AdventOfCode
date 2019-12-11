package com.lpetch.adventofcode.twenty19

import scala.io.Source

object DayOne extends App {
  def recursiveFuelRequirement(inputMass:Int): Int = {
    val fuelRequirement = (inputMass.toFloat / 3).toInt - 2
    if (fuelRequirement > 0) {
      fuelRequirement + recursiveFuelRequirement(fuelRequirement)
    } else {
      0
    }
  }

  def fuelValue(inputMass:Int): Int = {
    (inputMass.toFloat / 3).toInt - 2
  }

  val totalModuleMass = Source.fromFile("input/DayOne.txt").getLines.toArray
    .foldLeft(0)((running, current) =>  running + fuelValue(current.toInt) + recursiveFuelRequirement(fuelValue(current.toInt)))

  println("Total Module Mass:" + totalModuleMass)
}

// take its mass, divide by three, round down, and subtract 2.
