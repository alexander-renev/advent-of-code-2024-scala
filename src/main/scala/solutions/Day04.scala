package com.adventofcode
package solutions

import common.{Day, Input}

case class Position(x: Int, y: Int) {
  def +(other: Position): Position = {
    Position(x + other.x, y + other.y)
  }

  def -(other: Position): Position = {
    Position(x - other.x, y - other.y)
  }

  def range(direction: Position, count: Int): Iterable[Position] = {
    Seq.fill(count - 1) { direction }.scanLeft(this)(_ + _)
  }

  def adjacent(): Seq[Position] = {
    Seq(
      Position(x, y + 1),
      Position(x, y - 1),
      Position(x + 1, y),
      Position(x - 1, y)
    )
  }

  override def toString: String = f"($x,$y)"
}

class Day04 extends Day {
  override val input: Input = new Input(4)
  private val directions =
    for (x <- Seq(-1, 0, 1); y <- Seq(-1, 0, 1) if x != 0 || y != 0)
      yield Position(x, y)
  private val directions2 = Seq(Position(1, 1), Position(1, -1))

  override def solvePart1(input: String): Unit = {
    val chars = parseInput(input)
    val count = chars.keys.view
      .filter(chars(_) == 'X')
      .map(position =>
        directions
          .map(d =>
            position.range(d, 4).map(p => chars.getOrElse(p, '_')).mkString
          )
          .count(_ == "XMAS")
      )
      .sum
    println(f"Found $count words")
  }

  override def solvePart2(input: String): Unit = {
    val chars = parseInput(input)
    val count = chars.keys.view
      .count(position => {
        directions2
          .map(d => Seq(position - d, position, position + d))
          .map(pos => pos.map(p => chars.getOrElse(p, '_')).mkString)
          .forall(s => s == "MAS" || s == "SAM")
      })
    println(f"Found $count words")
  }

  private def parseInput(input: String) = {
    input
      .split("\n")
      .map(line => line.zipWithIndex)
      .zipWithIndex
      .flatMap(v => v._1.map(n => (Position(n._2, v._2), n._1)))
      .toMap
  }
}
