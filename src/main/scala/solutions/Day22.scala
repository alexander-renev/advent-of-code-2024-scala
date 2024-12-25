package com.adventofcode
package solutions

import scala.collection.mutable
import common.{Day, Input}

class Day22 extends Day {
  override val input: Input = new Input(22)

  override def solvePart1(input: String): Unit = {
    val numbers = parseInput(input)
    val secrets = numbers.map(num => {
      var current = num.toLong
      (1 to 2000).foreach(_ => current = nextNumber(current))
      current
    })
    println(f"Sum is ${secrets.sum}")
  }

  override def solvePart2(input: String): Unit = {
    val numbers = parseInput(input)
    val secrets = numbers.map(num =>
      List
        .iterate(nextNumber(num), 2000)(nextNumber)
        .map(i => (i % 10).toInt)
        .toArray
    )
    val results = mutable.Map[(Int, Int, Int, Int), Int]()
    secrets.foreach(s => describe(s, results))
    val maxResult = results.values.max
    println(f"Max result is $maxResult")
  }

  private def describe(
      numbers: Array[Int],
      results: mutable.Map[(Int, Int, Int, Int), Int]
  ): Unit = {
    val diffs = numbers.drop(1).zip(numbers).map(_ - _)
    val processed = mutable.Set[(Int, Int, Int, Int)]()
    (0 to (diffs.length - 4)).foreach(i => {
      val key = (diffs(i), diffs(i + 1), diffs(i + 2), diffs(i + 3))
      if (processed.add(key)) {
        results(key) = results.getOrElse(key, 0) + numbers(i + 4)
      }
    })
  }

  private def nextNumber(secret: Long): Long = {
    val step1 = prune(mix(secret * 64, secret))
    val step2 = prune(mix(step1 / 32, step1))
    prune(mix(step2, step2 * 2048))
  }

  private def mix(secret: Long, num: Long) = {
    secret ^ num
  }

  private def prune(secret: Long) = {
    secret % 16777216
  }

  private def parseInput(input: String) = {
    getLines(input).map(_.toInt)
  }
}
