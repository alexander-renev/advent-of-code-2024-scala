package com.adventofcode
package solutions

import common.{Day, Input}

class Day01 extends Day {
  override val input = Input(1)

  override def solvePart1(input: String): Unit = {
    val (list1, list2) = parseInput(input)
    val distance =
      list1.sorted.zip(list2.sorted).map((v1, v2) => (v1 - v2).abs).sum
    println(f"Distance is $distance")
  }

  override def solvePart2(input: String): Unit = {
    val (list1, list2) = parseInput(input)
    val counts = list2.groupBy(v => v).view.mapValues(_.length)
    val similarity = list1.map(v => v * counts.getOrElse(v, 0)).sum
    println(f"Similarity is $similarity")
  }

  private def parseInput(input: String) = {
    input
      .split("\n")
      .map(_.split("\\s+").map(_.toInt))
      .map(ar => (ar(0), ar(1)))
      .unzip
  }
}
