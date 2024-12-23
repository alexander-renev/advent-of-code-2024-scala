package com.adventofcode
package solutions

import scala.jdk.StreamConverters.*
import scala.collection.mutable
import common.{Day, Input}

class Day20 extends Day {
  override val input: Input = new Input(20)

  override def solvePart1(input: String): Unit = {
    val adjacent = getAdjacent(2)
    calculateCheats(input, adjacent)
  }

  private def calculateCheats(input: String, adjacent: List[Position]): Unit = {
    val (leastCount, rawGrid) = parseInput(input)
    val start = rawGrid.find(_._2 == 'S').get._1
    val finish = rawGrid.find(_._2 == 'E').get._1
    val grid = rawGrid.updated(start, '.').updated(finish, '.')
    val routes = buildRoutes(start, grid)
    val reverseRoutes = buildRoutes(finish, grid)
    val minLength = routes(finish)
    val requiredLength = minLength - leastCount

    val cheats = routes.keys
      .flatMap(p => {
        adjacent
          .map(p + _)
          .filter(reverseRoutes.contains)
          .filter(end =>
            reverseRoutes(end) + p.manhattanTo(end) + routes(
              p
            ) <= requiredLength
          )
          .map(adj => (p, adj))
          .distinct
      })
      .toList
      .distinct

    println(f"Cheats count ${cheats.size}")
  }

  private def buildRoutes(
      from: Position,
      grid: Map[Position, Char]
  ): Map[Position, Int] = {
    val result = mutable.Map[Position, Int]()
    val queue = mutable.ListBuffer[(Position, Int)]()

    result(from) = 0
    queue.append((from, 0))
    while (queue.nonEmpty) {
      val (pos, length) = queue.remove(0)
      val nextValues =
        pos
          .adjacent()
          .filter(p => grid.getOrElse(p, '#') == '.')
          .filter(p => !result.contains(p))
          .map((_, length + 1))

      queue.appendAll(nextValues)
      nextValues.foreach(p => result(p._1) = p._2)
    }
    result.toMap
  }

  private def getAdjacent(size: Int): List[Position] = {
    (-size to size)
      .flatMap(x => (-size to size).map(y => Position(x, y)))
      .filter(_ != Position(0, 0))
      .filter(p => p.manhattanTo(Position(0, 0)) <= size)
      .toList
  }

  override def solvePart2(input: String): Unit = {
    val adjacent = getAdjacent(20)
    calculateCheats(input, adjacent)
  }

  private def parseInput(input: String) = {
    val lines = input.lines().toScala(Array)
    val leastCount = lines(0).toInt
    val grid = lines
      .drop(1)
      .map(line => line.zipWithIndex)
      .zipWithIndex
      .flatMap(v => v._1.map(n => (Position(n._2, v._2), n._1)))
      .toMap
    (leastCount, grid)
  }
}
