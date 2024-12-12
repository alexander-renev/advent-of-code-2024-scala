package com.adventofcode
package solutions

import scala.jdk.StreamConverters.*
import scala.collection.mutable
import common.{Day, Input}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class Day12 extends Day {
  override val input: Input = new Input(12)

  override def solvePart1(input: String): Unit = {
    val grid = parseInput(input)
    val fence = getFenceSize(grid, 0, getPerimeter1)
    println(f"Fence price is $fence")
  }

  @tailrec
  private def getFenceSize(grid: Map[Position, Char], current: Int, calcPerimeter: (mutable.Set[Position],
    Map[Position, Char]) => Int): Int = {
    if (grid.isEmpty) {
      current
    } else {
      val (position, ch) = grid.head
      val positions = mutable.Set[Position](position)
      val queue = ListBuffer[Position]()
      queue.append(position)
      while (queue.nonEmpty) {
        val current = queue.remove(0)
        current
          .adjacent()
          .filter(p => !positions.contains(p))
          .filter(p => grid.getOrElse(p, '_') == ch)
          .foreach(p => {
            queue.append(p)
            positions.add(p)
          })
      }
      val square = positions.size
      val perimeter = calcPerimeter(positions, grid)
      val newGrid = grid.removedAll(positions)
      getFenceSize(newGrid, current + square * perimeter, calcPerimeter)
    }
  }

  override def solvePart2(input: String): Unit = {
    val grid = parseInput(input)
    val fence = getFenceSize(grid, 0, getPerimeter2)
    println(f"Fence price is $fence")
  }

  private def getPerimeter1(
      figure: mutable.Set[Position],
      grid: Map[Position, Char]
  ): Int = {
    figure.toList
      .map(p => p.adjacent().count(pt => !figure.contains(pt)))
      .sum
  }

  private def getPerimeter2(
      figure: mutable.Set[Position],
      grid: Map[Position, Char]
  ): Int = {
    val xRange = figure.map(_.x).min to figure.map(_.x).max
    val yRange = figure.map(_.y).min to figure.map(_.y).max

    val horizontalLines = yRange
      .map(y => {
        val positions =
          xRange.map(Position(_, y)).intersect(figure.toSeq).toList
        val vectors = Seq(Position(0, 1), Position(0, -1))
        vectors
          .map(vector => {
            val xs = positions
              .filter(p => grid(p) != grid.getOrElse(p + vector, '_'))
              .map(_.x)
              .sorted
            getRangesCount(xs)
          })
          .sum
      })
      .sum
    val verticalLines = xRange
      .map(x => {
        val positions =
          yRange.map(Position(x, _)).intersect(figure.toSeq).toList
        val vectors = Seq(Position(1, 0), Position(-1, 0))
        vectors
          .map(vector => {
            val ys = positions
              .filter(p => grid(p) != grid.getOrElse(p + vector, '_'))
              .map(_.y)
              .sorted
            getRangesCount(ys)
          })
          .sum
      })
      .sum
    val lines = verticalLines + horizontalLines
    lines
  }

  private def getRangesCount(src: List[Int]): Int = {
    if (src.isEmpty) 0
    else src.zip(src.tail).count(p => p._1 + 1 != p._2) + 1
  }

  private def parseInput(input: String) = {
    input
      .lines()
      .toScala(Array)
      .map(line => line.zipWithIndex)
      .zipWithIndex
      .flatMap(v => v._1.map(n => (Position(n._2, v._2), n._1)))
      .toMap
  }
}
