package com.lpetch.adventofcode.twenty19

import scala.collection.mutable

object DayFour extends App {

  def validateDigits(input: String, tokenPointer: Int = 0): Boolean = {
    if (input(tokenPointer) > input(tokenPointer + 1)) {
      false
    } else if (tokenPointer == input.length - 2) {
      true
    } else {
      validateDigits(input, tokenPointer + 1)
    }
  }

  def validatePairs(input: String, tokenPointer: Int = 0, digitCount: mutable.Map[Char, Int] = mutable.Map()): Boolean = {
    digitCount(input(tokenPointer)) = digitCount.getOrElse(input(tokenPointer), 0) + 1
    if (tokenPointer == input.length - 1) {
      digitCount.values.exists(_ == 2)
    } else {
      validatePairs(input, tokenPointer + 1, digitCount)
    }
  }

  var validPasswords = (367479 to 893698).foldLeft(0) { (validCount, current) =>
    if (validateDigits(current.toString) && validatePairs(current.toString)) {
      validCount + 1
    } else {
      validCount
    }
  }

  println("Valid count: " + validPasswords)
}
