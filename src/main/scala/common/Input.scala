package com.adventofcode.common

import scala.io.Source

class Input(day: Int) {
  private def getInput(name: String): String = {
    val fileName = f"inputs/day$day%02d/$name.txt"
    Source.fromResource(fileName).mkString
  }
  def getPart1Test: String = {
    getInput("test")
  }
  def getPart2Test: String = {
    getInput("test")
  }
  def getPart1Real: String = {
    getInput("real")
  }
  def getPart2Real: String = {
    getInput("real")
  }
}