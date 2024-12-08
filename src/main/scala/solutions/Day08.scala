package com.adventofcode
package solutions

import common.{Day, Input}

class Day08 extends Day {
  override val input: Input = new Input(8)

  override def solvePart1(input: String): Unit = {
    val board = parseInput(input)
    val positions = board.groupMap(_._2)(_._1)
    val antinodes = board.values.toList.distinct
      .filter(_ != '.')
      .flatMap(ch => {
        val antennas = positions(ch)
        antennas.toList
          .combinations(2)
          .map(l => (l.head, l.tail.head))
          .filter(p => p._1 != p._2)
          .flatMap(p => Seq(antinode1(p._1, p._2), antinode1(p._2, p._1)))
          .filter(board.contains)
      })
      .distinct
    val antinodesCount = antinodes.length
    println(f"Antinodes count $antinodesCount")
  }

  override def solvePart2(input: String): Unit = {
    val board = parseInput(input)
    val positions = board.groupMap(_._2)(_._1)
    val minX = board.keys.map(_.x).min
    val maxX = board.keys.map(_.x).max
    val range = minX to maxX
    val antinodes = board.values.toList.distinct
      .filter(_ != '.')
      .flatMap(ch => {
        val antennas = positions(ch)
        antennas.toList
          .combinations(2)
          .map(l => (l.head, l.tail.head))
          .filter(p => p._1 != p._2)
          .flatMap(p => antinodes2(p._1, p._2, range))
          .filter(board.contains)
      })
      .distinct
    val antinodesCount = antinodes.length
    println(f"Antinodes count $antinodesCount")
  }

  private def antinode1(pos1: Position, pos2: Position) = {
    Position(pos2.x * 2 - pos1.x, pos2.y * 2 - pos1.y)
  }

  private def antinodes2(pos1: Position, pos2: Position, boardSize: Range) = {
    val delta = pos2 - pos1
    Seq.unfold(pos2)(p => {
      val next = p + delta
      if (boardSize.contains(next.x) && boardSize.contains(next.y))
        Some(next, next)
      else None
    }) ++ Seq.unfold(pos1)(p => {
      val next = p - delta
      if (boardSize.contains(next.x) && boardSize.contains(next.y))
        Some(next, next)
      else None
    }) ++ Seq(pos1, pos2)
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
