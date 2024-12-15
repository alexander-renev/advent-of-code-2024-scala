package com.adventofcode
package solutions

import scala.math.Integral.Implicits._
import common.{Day, Input}

class Day13 extends Day {
  case class Machine(a: Position, b: Position, prize: Position)

  override val input: Input = new Input(13)

  override def solvePart1(input: String): Unit = {
    val machines = parseInput(input)
    val sum = machines
      .map(m => minPrice(m, BigInt(0)))
      .flatMap(Option.option2Iterable)
      .sum
    println(f"Sum is $sum")
  }

  override def solvePart2(input: String): Unit = {
    val machines = parseInput(input)
    val sum = machines
      .map(m => minPrice(m, BigInt("10000000000000")))
      .flatMap(Option.option2Iterable)
      .sum
    println(f"Sum is $sum")
  }

  private def minPrice(machine: Machine, delta: BigInt): Option[BigInt] = {
    val singleSolution =
      (machine.a.x * machine.b.y - machine.a.y * machine.b.x) != 0
    val prizeX = machine.prize.x + delta
    val prizeY = machine.prize.y + delta
    if (singleSolution) {
      val b =
        (prizeY * machine.a.x - prizeX * machine.a.y) / (machine.a.x * machine.b.y - machine.a.y * machine.b.x)
      val a = (prizeX - b * machine.b.x) / machine.a.x
      // We need integer values, so test this in case we have some rounding issues
      if (
        machine.a.x * a + machine.b.x * b == prizeX && machine.a.y * a + machine.b.y * b == prizeY
      ) {
        Some(a * 3 + b)
      } else {
        None
      }
    } else {
      // Test data does not contain multiple solution cases for some reason
      None
    }
  }

  private def parseInput(input: String) = {
    val reg = "\\d+".r
    parseLineGroups(input).map(group => {
      val numbers = group
        .map(line => reg.findAllIn(line).map(_.toInt).toArray)
        .map(ar => Position(ar(0), ar(1)))
      Machine(numbers(0), numbers(1), numbers(2))
    })
  }
}
