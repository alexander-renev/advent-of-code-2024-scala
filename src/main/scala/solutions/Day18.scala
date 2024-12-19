package com.adventofcode
package solutions

import scala.collection.mutable
import common.{Day, Input}

class Day18 extends Day {
  override val input: Input = new Input(18)

  override def solvePart1(input: String): Unit = {
    val (size, grid) = parseInput1(input)
    val length = calcLength(size, grid).get
    println(f"Length is $length")
  }

  private def calcLength(size: Int, grid: Set[Position]): Option[Int] = {
    val checkRange = inRange(size)
    val queue = mutable.ListBuffer[(Position, Int)]()
    val visited = mutable.Map[Position, Int]()
    queue.append((Position(0, 0), 0))
    visited(Position(0, 0)) = 0
    val finish = Position(size, size)
    while (queue.nonEmpty && queue.head._1 != finish) {
      val (pos, length) = queue.remove(0)
      visited(pos) = length
      val nextValues =
        pos
          .adjacent()
          .filter(checkRange)
          .filter(!grid.contains(_))
          .filter(p => !visited.contains(p))
          .map((_, length + 1))
      queue.appendAll(nextValues)
      nextValues.foreach(p => visited(p._1) = p._2)
    }
    queue.headOption.map(_._2)
  }

  private def inRange(size: Int)(p: Position) = {
    val sizeRange = (0 to size)
    sizeRange.contains(p.x) && sizeRange.contains(p.y)
  }

  override def solvePart2(input: String): Unit = {
    val (size, grid) = parseInput2(input)
    val firstIndex = grid.indices
      .find(index => {
        val part = grid.slice(0, index + 1).toSet
        calcLength(size, part).isEmpty
      })
      .map(grid(_))
      .get

    println(f"First byte is $firstIndex")
  }

  private def printBoard(board: Set[Position]): String = {
    val maxY = board.map(_.y).max
    val maxX = board.map(_.x).max

    (0 to maxY)
      .map(y => {
        (0 to maxX)
          .map(x => Position(x, y))
          .map(pos => if (board.contains(pos)) '#' else '.')
          .mkString
      })
      .mkString("\n")
  }

  private def parseInput1(input: String) = {
    val lines = input.split(raw"\r\n")
    val size = lines(0).toInt
    val bytes = lines(1).toInt
    val grid = lines.slice(2, bytes + 2).map(Position.fromString(_)).toSet
    (size, grid)
  }

  private def parseInput2(input: String) = {
    val lines = input.split(raw"\r\n")
    val size = lines(0).toInt
    val grid = lines.drop(2).map(Position.fromString(_)).toArray
    (size, grid)
  }
}
