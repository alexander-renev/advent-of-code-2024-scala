package com.adventofcode
package common

import scala.jdk.StreamConverters.*

trait Day {
  val input: Input
  def solvePart1(input: String): Unit
  def solvePart2(input: String): Unit
  def solve(): Unit = {
    println("Test data")
    solvePart1(input.getPart1Test)
    solvePart2(input.getPart2Test)

    println("Real data")
    //solvePart1(input.getPart1Real)
    //solvePart2(input.getPart2Real)
  }

  protected def parseLineGroups(input: String): Array[Array[String]] = {
    Seq
      .unfold(input.lines().toScala(Array))((list: Array[String]) => {
        if (list.length == 0) {
          None
        } else {
          val (first, second) = list.span(l => l != "")
          Some(first, second.drop(1))
        }
      })
      .toArray
  }
}
