package com.adventofcode
package solutions

import scala.collection.mutable
import common.{Day, Input}

class Day21 extends Day {
  override val input: Input = new Input(21)

  override def solvePart1(input: String): Unit = {
    val lines = parseInput(input)
    val cache = mutable.Map[(String, Int), BigInt]()
    val sum = lines.map(calculateComplexity(_, 2, cache)).sum
    println(f"Sum is $sum")
  }

  override def solvePart2(input: String): Unit = {
    val lines = parseInput(input)
    val cache = mutable.Map[(String, Int), BigInt]()
    val sum = lines.map(calculateComplexity(_, 25, cache)).sum
    println(f"Sum is $sum")
  }

  private def calculateComplexity(
      line: String,
      keyboards: Int,
      cache: mutable.Map[(String, Int), BigInt]
  ): BigInt = {
    val number = raw"\d+".r.findFirstMatchIn(line).get.toString().toInt
    val big = makeBigKeyboard()
    val bigMoves = getMoves(big)
    val little = makeLittleKeyboard()
    val littleMoves = getMoves(little)

    val initial = getCombinations(line, bigMoves)
    val total =
      initial
        .map(_.map(comb => getLength(comb, littleMoves, keyboards, cache)).min)
        .sum
    total * number
  }

  private def getLength(
      line: String,
      moves: Map[(String, String), List[String]],
      steps: Int,
      cache: mutable.Map[(String, Int), BigInt]
  ): BigInt = {
    if (steps == 0) {
      line.length
    } else {
      val fromCache = cache.get((line, steps))
      if (fromCache.isDefined) {
        fromCache.get
      } else {
        val combinations = getCombinations(line, moves)
        val result = combinations
          .map(_.map(comb => getLength(comb, moves, steps - 1, cache)).min)
          .sum
        cache((line, steps)) = result
        result
      }
    }
  }

  private def getCombinations(
      line: String,
      moves: Map[(String, String), List[String]]
  ): List[List[String]] = {
    val allMoves =
      line
        .zip("A" + line.take(line.length - 1))
        .toList
        .map(pair => {
          val (to, from) = (pair._1.toString, pair._2.toString)
          if (from == to) {
            List("A")
          } else {
            moves((from, to))
          }
        })
    allMoves
  }

  private val posToCommandMap = Map(
    Position(0, 1) -> "V",
    Position(0, -1) -> "^",
    Position(1, 0) -> ">",
    Position(-1, 0) -> "<"
  )

  private def getMoves(
      keyboard: Map[String, Position]
  ): Map[(String, String), List[String]] = {
    val positions = keyboard.values.toSet
    keyboard.keys.toList
      .combinations(2)
      .map(l => (l.head, l.tail.head))
      .flatMap(pair =>
        Seq(pair, (pair._2, pair._1))
          .map(pair => {
            val from = keyboard(pair._1)
            val to = keyboard(pair._2)
            val directionX =
              if (from.x < to.x) Position(1, 0) else Position(-1, 0)
            val directionY =
              if (from.y < to.y) Position(0, 1) else Position(0, -1)
            val repeatedX = List.fill((from.x - to.x).abs)(directionX)
            val repeatedY = List.fill((from.y - to.y).abs)(directionY)
            val commands = repeatedX.appendedAll(repeatedY)
            val allowed = commands.permutations.filter(cmds => {
              cmds.scan(from)(_ + _).forall(positions.contains)
            })
            (
              pair,
              allowed
                .map(_.map(posToCommandMap).appended("A").mkString)
                .toList
            )
          })
      )
      .toMap
  }

  private def makeBigKeyboard(): Map[String, Position] = {
    val result = mutable.Map[String, Position]()
    (0 to 2).foreach(x => {
      result((x + 7).toString) = Position(x, 0)
      result((x + 4).toString) = Position(x, 1)
      result((x + 1).toString) = Position(x, 2)
      result("0") = Position(1, 3)
      result("A") = Position(2, 3)
    })
    result.toMap
  }

  private def makeLittleKeyboard(): Map[String, Position] = {
    val result = mutable.Map[String, Position]()
    result("^") = Position(1, 0)
    result("A") = Position(2, 0)
    result("<") = Position(0, 1)
    result("V") = Position(1, 1)
    result(">") = Position(2, 1)
    result.toMap
  }

  private def parseInput(input: String) = {
    getLines(input)
  }
}
