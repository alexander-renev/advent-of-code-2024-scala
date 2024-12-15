package com.adventofcode
package solutions

import common.{Day, Input}

import scala.annotation.tailrec

class Day14 extends Day {
  case class Robot(position: Position, velocity: Position) {
    def move(dimensions: Position): Robot = {
      val newPositionRaw = position + velocity
      val newPosition = Position(
        normalize(newPositionRaw.x, dimensions.x),
        normalize(newPositionRaw.y, dimensions.y)
      )
      Robot(newPosition, velocity)
    }

    private def normalize(value: Int, upper: Int): Int = {
      if (value < 0) {
        value + upper
      } else if (value >= upper) {
        value - upper
      } else {
        value
      }
    }

    override def toString: String = f"$position"
  }

  override val input: Input = new Input(14)

  override def solvePart1(input: String): Unit = {
    val (robots, dimensions) = parseInput(input)
    var current = robots
    (1 to 100).foreach(_ => current = current.map(_.move(dimensions)))
    val quadrants = current
      .map(_.position)
      .map(p => {
        val q1 = if (p.x < dimensions.x / 2 && p.y < dimensions.y / 2) 1 else 0
        val q2 = if (p.x > dimensions.x / 2 && p.y < dimensions.y / 2) 1 else 0
        val q3 = if (p.x < dimensions.x / 2 && p.y > dimensions.y / 2) 1 else 0
        val q4 = if (p.x > dimensions.x / 2 && p.y > dimensions.y / 2) 1 else 0
        (q1, q2, q3, q4)
      })
      .fold((0, 0, 0, 0))((x1, x2) =>
        (x1._1 + x2._1, x1._2 + x2._2, x1._3 + x2._3, x1._4 + x2._4)
      )
    val factor = quadrants._1 * quadrants._2 * quadrants._3 * quadrants._4
    println(f"Factor is $factor")
  }

  override def solvePart2(input: String): Unit = {
    val (robots, dimensions) = parseInput(input)
    // Test data not contains christmas tree
    if (dimensions.x > 20) {
      val time = findTime(robots, dimensions, 0)
      println(f"Time required $time")
    }
  }

  @tailrec
  private def findTime(
      robots: List[Robot],
      dimensions: Position,
      elapsed: Int
  ): Int = {
    if (
      // Image "frame" contains a lot of xxxx
      printList(robots, dimensions).contains("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
    ) {
      elapsed
    } else {
      findTime(robots.map(_.move(dimensions)), dimensions, elapsed + 1)
    }
  }

  private def printList(robots: List[Robot], dimensions: Position): String = {
    val coords = robots.map(_.position).toSet

    (0 until dimensions.y)
      .map(y => {
        (0 until dimensions.x)
          .map(x => if (coords.contains(Position(x, y))) "x" else "_")
          .mkString
      })
      .mkString("\n")
  }

  private def parseInput(input: String) = {
    val lines = input.split('\n')
    val number = raw"-?\d+".r
    val sizes = number.findAllMatchIn(lines(0)).map(_.group(0).toInt).toArray
    val dimensions = Position(sizes(0), sizes(1))
    val robots = lines
      .drop(1)
      .map(line => {
        val matches = number.findAllMatchIn(line).map(_.group(0).toInt).toArray
        Robot(
          Position(matches(0), matches(1)),
          Position(matches(2), matches(3))
        )
      })
      .toList
    (robots, dimensions)
  }
}
