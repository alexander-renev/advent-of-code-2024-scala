package com.adventofcode
package solutions

import common.{Day, Input}

import scala.annotation.tailrec

class Day15 extends Day {
  override val input: Input = new Input(15)

  override def solvePart1(input: String): Unit = {
    val (rawBoard, commands) = parseInput(input)
    val robot = rawBoard.find(_._2 == '@').map(_._1).get
    val board = rawBoard.updated(robot, '.')
    val (_, resultBoard) = processCommands(commands.toList, robot, board)
    val boxes =
      resultBoard.filter(_._2 == 'O').map(p => p._1.x + 100 * p._1.y).sum
    println(f"Sum of coordinates is $boxes")
  }

  @tailrec
  private def processCommands(
      commands: List[Position],
      current: Position,
      board: Map[Position, Char]
  ): (Position, Map[Position, Char]) = {
    if (commands.isEmpty) {
      (current, board)
    } else {
      val nextPosition = current + commands.head
      if (board(nextPosition) == '#') {
        // cannot move
        processCommands(commands.tail, current, board)
      } else if (board(nextPosition) == '.') {
        // can just move
        processCommands(commands.tail, nextPosition, board)
      } else {
        // got box - should try to move
        val emptySpace = Iterator
          .iterate(nextPosition)(_ + commands.head)
          .takeWhile(board(_) != '#')
          .find(board(_) == '.')
        if (emptySpace.isDefined) {
          processCommands(
            commands.tail,
            nextPosition,
            board.updated(nextPosition, '.').updated(emptySpace.get, 'O')
          )
        } else {
          // Nowhere to move
          processCommands(commands.tail, current, board)
        }
      }
    }
  }

  private def printBoard(
      board: Map[Position, Char],
      robot: Position,
      direction: Position
  ): String = {
    val maxY = board.keys.map(_.y).max
    val maxX = board.keys.map(_.x).max
    val directionName = direction.toDirection

    (0 to maxY)
      .map(y => {
        (0 to maxX)
          .map(x => Position(x, y))
          .map(pos => if (pos == robot) '@' else board(pos))
          .mkString
      })
      .mkString("\n") + f"\n$directionName"
  }

  override def solvePart2(input: String): Unit = {
    val (rawBoard, commands) = parseInput2(input)
    val robot = rawBoard.find(_._2 == '@').map(_._1).get
    val board = rawBoard.updated(robot, '.')
    val (_, resultBoard) = processCommands2(commands.toList, robot, board)
    val boxes =
      resultBoard.filter(p => p._2 == '[').map(p => p._1.x + 100 * p._1.y).sum
    println(f"Sum of coordinates is $boxes")
  }

  @tailrec
  private def processCommands2(
      commands: List[Position],
      current: Position,
      board: Map[Position, Char]
  ): (Position, Map[Position, Char]) = {

    if (commands.isEmpty) {
      (current, board)
    } else {
      val nextPosition = current + commands.head
      if (board(nextPosition) == '#') {
        // cannot move
        processCommands2(commands.tail, current, board)
      } else if (board(nextPosition) == '.') {
        // can just move
        processCommands2(commands.tail, nextPosition, board)
      } else {
        // got box - should try to move
        if (commands.head.y == 0) {
          // horizontal move
          val y = nextPosition.y
          val emptySpace = Iterator
            .iterate(nextPosition)(_ + commands.head)
            .takeWhile(board(_) != '#')
            .find(board(_) == '.')
          if (emptySpace.isDefined) {
            var newBoard = board
            val sign = if (nextPosition.x < emptySpace.get.x) -1 else 1
            (emptySpace.get.x until nextPosition.x by sign).foreach(x => {
              newBoard = newBoard.updated(
                Position(x, y),
                newBoard(Position(x + sign, y))
              )
            })
            newBoard = newBoard.updated(nextPosition, '.')

            processCommands2(
              commands.tail,
              nextPosition,
              newBoard
            )
          } else {
            // Nowhere to move
            processCommands2(commands.tail, current, board)
          }
        } else {
          val dependentNodes =
            getDependent(List(nextPosition), board, commands.head)
          if (dependentNodes.isEmpty) {
            processCommands2(commands.tail, current, board)
          } else {
            val dependent = dependentNodes.get
            val moved = dependent.map(_ + commands.head)
            val cleared = dependent.filter(moved.contains(_) == false)
            val afterMove = dependent
              .map(p => (p, board(p)))
              .foldLeft(board)((b, pair) =>
                b.updated(pair._1 + commands.head, pair._2)
              )
            val afterClear =
              cleared.foldLeft(afterMove)((b, pos) => b.updated(pos, '.'))
            processCommands2(commands.tail, nextPosition, afterClear)
          }
        }
      }
    }
  }

  private def parseInput(input: String) = {
    val parts = input.split(raw"\r\n\r\n")
    val board = parts(0)
      .split(raw"\r\n")
      .map(line => line.zipWithIndex)
      .zipWithIndex
      .flatMap(v => v._1.map(n => (Position(n._2, v._2), n._1)))
      .toMap

    val commands = parts(1)
      .replaceAll(raw"\r\n", "")
      .map {
        case 'v' => Position(0, 1)
        case '^' => Position(0, -1)
        case '>' => Position(1, 0)
        case '<' => Position(-1, 0)
        case ch  => throw RuntimeException(f"Unknown char $ch")
      }

    (board, commands)
  }

  @tailrec
  private def getDependent(
      nextPosition: List[Position],
      board: Map[Position, Char],
      command: Position
  ): Option[List[Position]] = {
    val neighbours = nextPosition
      .flatMap(pos => {
        if (board(pos) == ']') Seq(pos, Position(pos.x - 1, pos.y))
        else Seq(pos, Position(pos.x + 1, pos.y))
      })
      .toSet

    val moved =
      neighbours.toList
        .map(_ + command)
        .filter(p => !neighbours.contains(p) && board(p) != '.')
    val all = (moved ++ neighbours).distinct
    if (moved.isEmpty) {
      Some(all)
    } else if (moved.exists(board(_) == '#')) {
      None
    } else {
      getDependent(all, board, command)
    }
  }

  private def parseInput2(input: String) = {
    val parts = input.split(raw"\r\n\r\n")
    val board = parts(0)
      .split(raw"\r\n")
      .map(line =>
        line.toList
          .flatMap(
            _ match {
              case '#' => Seq('#', '#')
              case 'O' => Seq('[', ']')
              case '.' => Seq('.', '.')
              case '@' => Seq('@', '.')
            }
          )
          .zipWithIndex
      )
      .zipWithIndex
      .flatMap(v => v._1.map(n => (Position(n._2, v._2), n._1)))
      .toMap

    val commands = parts(1)
      .replaceAll(raw"\r\n", "")
      .map {
        case 'v' => Position(0, 1)
        case '^' => Position(0, -1)
        case '>' => Position(1, 0)
        case '<' => Position(-1, 0)
        case ch  => throw RuntimeException(f"Unknown char $ch")
      }

    (board, commands)
  }
}
