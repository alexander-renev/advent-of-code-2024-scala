package com.adventofcode
package solutions

import scala.collection.mutable
import common.{Day, Input}

class Day11 extends Day {
  override val input: Input = new Input(11)

  override def solvePart1(input: String): Unit = {
    val stones = parseInput(input)
    val cache = mutable.Map[(BigInt, Int), BigInt]()
    val length = stones.map(s => processStones(cache, s, 25)).sum
    println(f"Length is $length")
  }

  override def solvePart2(input: String): Unit = {
    val stones = parseInput(input)
    val cache = mutable.Map[(BigInt, Int), BigInt]()
    val length = stones.map(s => processStones(cache, s, 75)).sum
    println(f"Length is $length")
  }

  private def processStones(
      cache: mutable.Map[(BigInt, Int), BigInt],
      stone: BigInt,
      steps: Int
  ): BigInt = {
    if (steps == 0) {
      1
    } else if (cache.contains((stone, steps))) {
      cache((stone, steps))
    } else {
      val result = stone match {
        case s if s == 0 => processStones(cache, 1, steps - 1)
        case s if s.toString().length % 2 == 0 =>
          val repr = s.toString()
          val length = repr.length
          processStones(
            cache,
            BigInt(repr.slice(0, length / 2)),
            steps - 1
          ) + processStones(
            cache,
            BigInt(repr.slice(length / 2, length)),
            steps - 1
          )
        case _ =>
          processStones(cache, stone * 2024, steps - 1)
      }
      cache((stone, steps)) = result
      result
    }
  }

  private def parseInput(input: String) = {
    input.split(' ').map(BigInt(_))
  }
}
