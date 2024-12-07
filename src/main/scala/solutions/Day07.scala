package com.adventofcode
package solutions

import common.{Day, Input}

import scala.annotation.tailrec

class Day07 extends Day {
  override val input: Input = new Input(7)

  override def solvePart1(input: String): Unit = {
    val operations: List[(BigInt, BigInt) => BigInt] = List(_ + _, _ * _)
    val result = getCalibrationResult(input, operations)
    println(f"Calibration result $result")
  }

  override def solvePart2(input: String): Unit = {
    val operations: List[(BigInt, BigInt) => BigInt] =
      List(_ + _, _ * _, (i1, i2) => BigInt(i1.toString + i2.toString))
    val result = getCalibrationResult(input, operations)
    println(f"Calibration result $result")
  }

  private def getCalibrationResult(
      input: String,
      operations: List[(BigInt, BigInt) => BigInt]
  ) = {
    val source = parseInput(input)
    source
      .filter(s =>
        possibleResults(s._2.toList.tail, List(s._2(0)), operations)
          .contains(s._1)
      )
      .map(_._1)
      .sum
  }

  @tailrec
  private def possibleResults(
      operands: List[BigInt],
      results: List[BigInt],
      operations: List[(BigInt, BigInt) => BigInt]
  ): List[BigInt] = {
    if (operands.isEmpty) {
      results
    } else {
      val head = operands.head
      val newResults = results.flatMap(r => operations.map(op => op(r, head)))
      possibleResults(operands.tail, newResults, operations)
    }
  }

  private def parseInput(input: String) = {
    input
      .split("\n")
      .map(line => {
        val parts = line.split(':').map(_.trim)
        val result = BigInt(parts(0))
        val operands = parts(1).split(' ').map(BigInt(_))
        (result, operands)
      })
  }
}
