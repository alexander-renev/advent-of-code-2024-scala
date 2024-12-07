package com.adventofcode
package solutions

import common.{Day, Input}

import scala.annotation.tailrec

class Day06 extends Day {
  private object MoveResult extends Enumeration {
    val Cycle, OutOfBoard = Value
  }

  override val input: Input = new Input(6)

  private val directions = Array[Position](
    Position(0, -1),
    Position(1, 0),
    Position(0, 1),
    Position(-1, 0)
  )

  private val directionsMap =
    directions.zip(directions.drop(1).appended(directions(0))).toMap

  override def solvePart1(input: String): Unit = {
    val board = parseInput(input)
    val agent = board.find(_._2 == '^').get._1

    val count = getVisited(
      agent,
      directions(0),
      board.updated(agent, '.'),
      List(agent)
    ).distinct.length
    println(f"Visited $count nodes")
  }

  override def solvePart2(input: String): Unit = {
    val board = parseInput(input)
    val agent = board.find(_._2 == '^').get._1
    val newBoard = board.updated(agent, '.')
    val visited = getVisited(
      agent,
      directions(0),
      board.updated(agent, '.'),
      List(agent)
    ).distinct

    val obstaclesCount = newBoard
      .filter(_._1 != agent)
      .filter(_._2 == '.')
      .keys
      .filter(visited.contains)
      .count(pos => {
        val updatedBoard = newBoard.updated(pos, '#')
        getMoveResult(
          agent,
          directions(0),
          updatedBoard
        ) == MoveResult.Cycle
      })

    println(f"Valid positions $obstaclesCount nodes")
  }

  @tailrec
  private def getVisited(
      position: Position,
      direction: Position,
      board: Map[Position, Char],
      visitedList: List[Position]
  ): List[Position] = {
    move(position, direction, board) match {
      case None         => visitedList
      case Some((p, d)) => getVisited(p, d, board, p :: visitedList)
    }
  }

  @tailrec
  private def getMoveResult(
      position: Position,
      direction: Position,
      board: Map[Position, Char],
      visited: Set[(Position, Position)] = Set()
  ): MoveResult.Value = {
    move(position, direction, board) match {
      case None => MoveResult.OutOfBoard
      case Some(pair) =>
        if (visited.contains(pair)) MoveResult.Cycle
        else getMoveResult(pair._1, pair._2, board, visited + pair)
    }
  }

  private def move(
      position: Position,
      direction: Position,
      board: Map[Position, Char]
  ) = {
    val nextPosition = position + direction
    val boardValue = board.get(nextPosition)
    if (boardValue.isEmpty) {
      None
    } else {
      if (boardValue.get == '#') {
        Some(position, directionsMap(direction))
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
