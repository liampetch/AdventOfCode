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

  def run(program: Array[Int], pos: Int = 0): Int = {
    program(pos) match {
      case 1 => program(getStorageLocation(pos, program)) = add(pos, program)
        run(program, pos + 4)
      case 2 => program(getStorageLocation(pos, program)) = multiply(pos, program)
        run(program, pos + 4)
      case 99 => program(0)
      case _ => -1
    }
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
