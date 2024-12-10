package com.adventofcode
package solutions

import common.{Day, Input}
import scala.collection.mutable
import scala.jdk.StreamConverters.*

class Day10 extends Day {
  override val input: Input = new Input(10)

  override def solvePart1(input: String): Unit = {
    val src = parseInput(input)
    val trailheads = src.filter(p => p._2 == 0)
    val scores = trailheads
      .map(hd => {
        val visited = mutable.Set(hd._1)
        val tops = mutable.Set[Position]()
        val queue = mutable.ListBuffer(hd._1)
        while (queue.nonEmpty) {
          val first = queue.remove(0)
          val height = src(first)

          if (height == 9) {
            tops.add(first)
          } else {
            val neighbours = List(
              Position(first.x, first.y + 1),
              Position(first.x, first.y - 1),
              Position(first.x + 1, first.y),
              Position(first.x - 1, first.y)
            )
              .filter(src.contains)
              .filter(p => !visited.contains(p))
              .filter(p => src(p) == height + 1)
            queue.appendAll(neighbours)
          }
        }
        tops.size
      })
      .sum

    println(f"Total score is $scores")
  }

  override def solvePart2(input: String): Unit = {
    val src = parseInput(input)
    val trailheads = src.filter(p => p._2 == 0).keys.toList
    val routes = mutable.Map[Int, List[List[Position]]]()
    routes(0) = trailheads.map(List(_))
    (1 to 9).foreach(h => {
      val previousRoutes = routes(h - 1)
      val newRoutes = previousRoutes.flatMap(rt => {
        val last = rt.last
        val neighbours = List(
          Position(last.x, last.y + 1),
          Position(last.x, last.y - 1),
          Position(last.x + 1, last.y),
          Position(last.x - 1, last.y)
        )
          .filter(src.contains)
          .filter(p => src(p) == h)
        neighbours.map(rt.appended)
      })
      routes(h) = newRoutes
    })
    val routesCount = routes(9).length
    println(f"Total score is $routesCount")
  }

  private def parseInput(input: String) = {
    input
      .lines()
      .toScala(Array)
      .map(line => line.zipWithIndex)
      .zipWithIndex
      .flatMap(v => v._1.map(n => (Position(n._2, v._2), n._1.toString.toInt)))
      .toMap
  }
}
