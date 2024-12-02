package com.adventofcode
package solutions

import common.{Day, Input}

class Day02 extends Day {
  override val input = Input(2)

  override def solvePart1(input: String): Unit = {
    val reports = parseInput(input)
    val count = reports.count(isValid)
    println(f"Valid reports count $count")
  }

  override def solvePart2(input: String): Unit = {
    val reports = parseInput(input)
    val count = reports.count(isValidExtended)
    println(f"Valid reports count $count")
  }

  private def parseInput(input: String) = {
    input
      .split("\n")
      .map(_.split("\\s+").map(_.toInt))
  }

  private def isValid(report: Array[Int]) = {
    val sign = if (report(0) - report(1) > 0) 1 else -1
    report
      .zip(report.drop(1))
      .forall((v1, v2) =>
        (1 to 3).contains((v1 - v2).abs) && ((sign == 1) == (v1 > v2))
      )
  }

  private def isValidExtended(report: Array[Int]) = {
    if (isValid(report)) true
    else {
      report.indices.exists(index =>
        isValid(
          report.zipWithIndex
            .filter((_, ind) => ind != index)
            .map((value, _) => value)
        )
      )
    }
  }
}
