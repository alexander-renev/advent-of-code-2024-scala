package com.adventofcode
package solutions

import common.{Day, Input}

import scala.annotation.tailrec

class Day06 extends Day {
  object MoveResult extends Enumeration {
    val Cycle, OutOfBoard = Value
  }

  override val input: Input = new Input(6)

  private val directions = Array[Position](
    Position(0, -1),
    Position(1, 0),
    Position(0, 1),
    Position(-1, 0)
  )

  override def solvePart1(input: String): Unit = {
    val board = parseInput(input)
    val infiniteDirections = Iterator.continually(directions).flatten
    val agent = board.find(_._2 == '^').get._1

    val count = getVisited(
      agent,
      infiniteDirections.next(),
      board.updated(agent, '.'),
      infiniteDirections,
      List(agent)
    ).distinct.length
    println(f"Visited $count nodes")
  }

  override def solvePart2(input: String): Unit = {
    val board = parseInput(input)
    val agent = board.find(_._2 == '^').get._1
    val newBoard = board.updated(agent, '.')
    val infiniteDirections = Iterator.continually(directions).flatten
    val visited = getVisited(
      agent,
      infiniteDirections.next(),
      board.updated(agent, '.'),
      infiniteDirections,
      List(agent)
    ).distinct

    val obstaclesCount = newBoard
      .filter(_._1 != agent)
      .filter(_._2 == '.')
      .keys
      .filter(visited.contains)
      .count(pos => {
        val updatedBoard = newBoard.updated(pos, '#')
        val infiniteDirections = Iterator.continually(directions).flatten
        getMoveResult(
          agent,
          infiniteDirections.next(),
          updatedBoard,
          infiniteDirections
        ) == MoveResult.Cycle
      })

    println(f"Valid positions $obstaclesCount nodes")
  }

  @tailrec
  private def getVisited(
      position: Position,
      direction: Position,
      board: Map[Position, Char],
      directions: Iterator[Position],
      visitedList: List[Position]
  ): List[Position] = {
    move(position, direction, board, directions) match {
      case None         => visitedList
      case Some((p, d)) => getVisited(p, d, board, directions, p :: visitedList)
    }
  }

  @tailrec
  private def getMoveResult(
      position: Position,
      direction: Position,
      board: Map[Position, Char],
      directions: Iterator[Position],
      visited: Set[(Position, Position)] = Set()
  ): MoveResult.Value = {
    move(position, direction, board, directions) match {
      case None => MoveResult.OutOfBoard
      case Some(pair) =>
        if (visited.contains(pair)) MoveResult.Cycle
        else getMoveResult(pair._1, pair._2, board, directions, visited + pair)
    }
  }

  private def move(
      position: Position,
      direction: Position,
      board: Map[Position, Char],
      directions: Iterator[Position]
  ) = {
    val nextPosition = position + direction
    val boardValue = board.get(nextPosition)
    if (boardValue.isEmpty) {
      None
    } else {
      if (boardValue.get == '#') {
        val newDirection = directions.next()
        Some(position, newDirection)
      } else {
        Some(nextPosition, direction)
      }
    }
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
