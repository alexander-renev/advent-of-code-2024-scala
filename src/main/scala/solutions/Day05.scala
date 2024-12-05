package com.adventofcode
package solutions

import common.{Day, Input}

class Day05 extends Day {
  override val input: Input = new Input(5)

  override def solvePart1(input: String): Unit = {
    val (orders, sources) = parseInput(input)
    val validSources = sources.filter(isValid(orders))
    val sumMiddle = validSources.map(s => s(s.length / 2)).sum
    println(f"Sum is $sumMiddle")
  }

  override def solvePart2(input: String): Unit = {
    val (orders, sources) = parseInput(input)
    val newValidSources = sources
      .filter(line => !isValid(orders)(line))
      .map(makeValid(orders))
    val sumMiddle = newValidSources.map(s => s(s.length / 2)).sum
    println(f"Sum is $sumMiddle")
  }

  private def makeValid(orders: Map[Int, Array[Int]])(
      source: Array[Int]
  ): Array[Int] = {
    val significantRules = orders
      .filter(p => source.contains(p._1))
      .view
      .mapValues(v => v.filter(n => source.contains(n)))
      .toMap
    source.sortWith((i1, i2) => {
      significantRules
        .getOrElse(i1, Array.empty[Int])
        .contains(i2) || !(significantRules
        .getOrElse(i2, Array.empty[Int])
        .contains(i1))
    })
  }

  private def isValid(orders: Map[Int, Array[Int]])(source: Array[Int]) = {
    val indexed = source.zipWithIndex.toMap
    source.zipWithIndex.forall(p => {
      orders
        .get(p._1)
        .forall(following => {
          following.forall(num => indexed.get(num).forall(pos => pos > p._2))
        })
    })
  }

  private def parseInput(input: String) = {
    val parts = input.split(raw"\n\n")
    val pairs = parts(0)
      .split('\n')
      .map(s => s.split('|').map(_.toInt))
      .map(p => (p(0), p(1)))
    val sources = parts(1).split('\n').map(s => s.split(',').map(_.toInt))
    (pairs.groupBy(_._1).map(p => (p._1, p._2.map(_._2))), sources)
  }
}
