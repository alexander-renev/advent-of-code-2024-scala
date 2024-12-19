package com.adventofcode
package solutions

import scala.collection.mutable
import common.{Day, Input}

class Day19 extends Day {
  override val input: Input = new Input(19)

  override def solvePart1(input: String): Unit = {
    val (patterns, towels) = parseInput(input)
    val matchCount = towels.count(matches(_, patterns))
    println(f"Matches count $matchCount")
  }

  private def matches(towel: String, patterns: List[String]): Boolean = {
    patterns.exists(pattern => {
      if (!towel.startsWith(pattern)) { false }
      else if (towel.length == pattern.length) {
        true
      } else {
        val remain = towel.substring(pattern.length)
        matches(remain, patterns)
      }
    })
  }

  private def matchesCount(
      towel: String,
      patterns: List[String],
      cache: mutable.Map[String, BigInt]
  ): BigInt = {
    val fromCache = cache.get(towel)
    if (fromCache.isDefined) {
      fromCache.get
    } else {
      val result = patterns
        .map(pattern => {
          if (!towel.startsWith(pattern)) {
            BigInt(0)
          } else if (towel.length == pattern.length) {
            BigInt(1)
          } else {
            val remain = towel.substring(pattern.length)
            matchesCount(remain, patterns, cache)
          }
        })
        .sum
      cache(towel) = result
      result
    }
  }

  override def solvePart2(input: String): Unit = {
    val (patterns, towels) = parseInput(input)
    val cache = mutable.Map[String, BigInt]()
    val matchCount = towels.map(matchesCount(_, patterns, cache)).sum
    println(f"Matches count $matchCount")
  }

  private def parseInput(input: String) = {
    val parts = input.split(raw"\r\n\r\n")
    val patterns = parts(0).split(',').map(_.trim).toList
    val towels = parts(1).split(raw"\r\n").toList
    (patterns, towels)
  }
}
