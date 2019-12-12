package com.lpetch.adventofcode.twenty19

import scala.io.Source

object DayTwo extends App {
  val IntCodeProgram = Source.fromFile("input/DayTwo.txt").getLines.next().split(",").map(_.toInt)

  def initProgram(program:Array[Int], noun:Int, verb:Int): Array[Int] = {
    program(1) = noun
    program(2) = verb
    program
  }

  def operandOne(pos:Int, program: Array[Int]) = program(program(pos + 1))
  def operandTwo(pos:Int, program: Array[Int]) = program(program(pos + 2))
  def getStorageLocation(pos:Int, program: Array[Int]) = program(pos + 3)
  def add(pos:Int, program: Array[Int]) = operandOne(pos, program) + operandTwo(pos, program)
  def multiply(pos:Int, program: Array[Int]) = operandOne(pos, program) * operandTwo(pos, program)

  def run(program:Array[Int]): Int = {
    var pos = 0

    while (true) {
      if (program(pos) == 1) {
        program(getStorageLocation(pos, program)) = add(pos, program)
        pos += 4
      } else if (program(pos) == 2) {
        program(getStorageLocation(pos, program)) = multiply(pos, program)
        pos += 4
      } else if (program(pos) == 99) {
        return program(0)
      } else {
        println("Something went wrong")
        return -1
      }
    }
    0
  }

  for (
    noun <- 1 to 99;
    verb <- 1 to 99
  ) {
    if (run(initProgram(IntCodeProgram.clone(), noun, verb)) == 19690720) {
      println("noun:" + noun + " verb: " + verb)
      println(100 * noun + verb)
      System.exit(0)
    }
  }
}

// take its mass, divide by three, round down, and subtract 2.
