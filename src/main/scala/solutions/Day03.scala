package com.adventofcode
package solutions

import common.{Day, Input}

class Day03 extends Day {
  sealed trait Command {
    def product: Int = 0
  }

  case class Multiply(arg1: Int, arg2: Int) extends Command {
    override def product: Int = arg1 * arg2
  }

  case class Toggle(enabled: Boolean) extends Command

  override val input: Input = new Input(3) {
    override def getPart2Test: String = getInput("test-2")
  }

  override def solvePart1(input: String): Unit = {
    val commands = parseInput(input)
    val sum = commands.map(_.product).sum
    println(f"Sum is $sum")
  }

  override def solvePart2(input: String): Unit = {
    val commands = parseInput(input)
    val (sum, _) = commands.foldLeft((0, true))((counter, value) => {
      val (total, enabled) = counter
      value match {
        case Multiply(_, _) =>
          if (enabled) (total + value.product, enabled) else (total, enabled)
        case Toggle(flag) => (total, flag)
      }
    })
    println(f"Sum is $sum")
  }

  private def parseInput(input: String) = {
    raw"(mul\((?<arg1>\d+),(?<arg2>\d+)\))|(do\(\))|(don't\(\))".r
      .findAllMatchIn(input)
      .map(m =>
        m.group(0) match {
          case "do()"    => Toggle(true)
          case "don't()" => Toggle(false)
          case _         => Multiply(m.group("arg1").toInt, m.group("arg2").toInt)
        }
      )
      .toList
  }
}
