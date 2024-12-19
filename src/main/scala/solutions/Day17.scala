package com.adventofcode
package solutions

import scala.collection.mutable
import scala.annotation.tailrec
import scala.jdk.StreamConverters.*
import common.{Day, Input}

class Day17 extends Day {
  private case class State(
      var regA: BigInt,
      var regB: BigInt,
      var regC: BigInt,
      instructions: Array[Int]
  ) {

    private val output: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer[Int]()
    private var instructionPointer: Int = 0

    override def toString: String = f"(A=$regA, B=$regB, C=$regC)"

    def getOutput: String = {
      output.mkString(",")
    }

    def checkOutput(test: Array[Int]): Boolean = {
      test.length == output.length && test.zip(output).forall(p => p._1 == p._2)
    }

    def getCombo(operand: Int): BigInt = {
      operand match
        case lit if (0 to 3).contains(lit) => lit
        case 4 => regA
        case 5 => regB
        case 6 => regC
        case 7 => throw RuntimeException("7 cannot be combo operand")
    }

    @tailrec
    final def run(): Unit = {
      if (instructions.indices.contains(instructionPointer + 1)) {
        // println(f"${Integer.toString(regA, 8)}\t${Integer.toString(regB, 8)}\t${Integer.toString(regC, 8)}\t${output.mkString(",")}")
        val instruction = instructions(instructionPointer)
        val operand = instructions(instructionPointer + 1)
        if (instruction == 0) {
          regA = regA / big2Power(getCombo(operand))
          instructionPointer += 2
          run()
        } else if (instruction == 1) {
          regB = regB ^ operand
          instructionPointer += 2
          run()
        } else if (instruction == 2) {
          regB = getCombo(operand) % 8
          instructionPointer += 2
          run()
        } else if (instruction == 3) {
          if (regA == 0) {
            instructionPointer += 2
            run()
          } else {
            instructionPointer = operand
            run()
          }
        } else if (instruction == 4) {
          regB = regB ^ regC
          instructionPointer += 2
          run()
        } else if (instruction == 5) {
          output.addOne((getCombo(operand) % 8).toInt)
          instructionPointer += 2
          run()
        } else if (instruction == 6) {
          regB = regA / big2Power(getCombo(operand))
          instructionPointer += 2
          run()
        } else if (instruction == 7) {
          regC = regA / big2Power(getCombo(operand))
          instructionPointer += 2
          run()
        } else {
          throw RuntimeException(f"Unknown instruction $instruction")
        }
      }
    }
  }

  private val powerCache = mutable.Map[BigInt, BigInt]()
  private def big2Power(n: BigInt): BigInt = {
    if (powerCache.contains(n)) {
      powerCache(n)
    } else {
      var result = BigInt(1)
      var count = BigInt(0)
      while (count < n) {
        result *= 2
        count += 1
      }
      powerCache(n) = result
      result
    }
  }

  override val input: Input = new Input(17)
  {
    override def getPart2Test: String = getInput("test2")
  }

  override def solvePart1(input: String): Unit = {
    val state = parseInput(input)
    state.run()
    println(state.getOutput)
  }

  override def solvePart2(input: String): Unit = {
    val state = parseInput(input)
    // Ignore test data
    if (state.regA > 10000) {
      var foundNumbers: List[String] = List("")
      // Build register A as 8-based number one digit by one
      state.instructions.indices.foreach(toIndex => {
        val ranged = state.instructions.drop(state.instructions.length - toIndex - 1)
        val testNumbers = (0 to 7).flatMap(digit => foundNumbers.map(n => f"$n$digit")).map(BigInt(_, 8))
        val newFoundNumbers = testNumbers.flatMap(testNumber => {
          val newState = State(testNumber, state.regB, state.regC, state.instructions)
          newState.run()
          if (newState.checkOutput(ranged)) {
            Some(testNumber)
          } else {
            None
          }
        }).map(_.toString(8)).toList
        foundNumbers = newFoundNumbers
      })
      val values = foundNumbers.map(BigInt(_, 8))
      println(values.min)
    }
  }

  private def parseInput(input: String) = {
    val num = raw"\d+".r
    val lines = input.lines().toScala(Array)
    val regs = lines.take(3).map(line => num.findFirstMatchIn(line).get.group(0).toInt)
    val program = num.findAllIn(lines.last).toList.map(_.toInt)
    State(regs(0), regs(1), regs(2), program.toArray)
  }
}
