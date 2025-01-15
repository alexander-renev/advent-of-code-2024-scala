package com.adventofcode
package solutions

import common.{Day, Input}

class Day25 extends Day {
  override val input: Input = new Input(25)

  override def solvePart1(input: String): Unit = {
    val (locks, keys) = parseInput(input)
    val lockDescs = locks.map(describe(_, false))
    val keyDescs = keys.map(describe(_, true))
    val matches = (
      for l <- lockDescs
          k <- keyDescs
          yield l.zip(k)
      ).count(_.forall(p => p._1 + p._2 < 6))

    println(f"Matches count $matches")
  }

  override def solvePart2(input: String): Unit = {}

  private def describe(
      item: Map[Position, Char],
      isKey: Boolean
  ): Array[Int] = {
    def positions(x: Int) = {
      if (isKey) {
        (5 to 0 by -1).map(Position(x, _))
      } else {
        (1 to 6).map(Position(x, _))
      }
    }
    (0 to 4)
      .map(x => positions(x).takeWhile(p => item(p) == '#').length)
      .toArray
  }

  private def parseInput(input: String) = {
    val items = parseLineGroups(input).map(grp => {
      val map = grp.indices
        .flatMap(y => grp(y).indices.map(x => (Position(x, y), grp(y)(x))))
        .toMap
      (grp(0) == "#####", map)
    })
    items.partitionMap(grp => if (grp._1) Left(grp._2) else Right(grp._2))
  }
}
