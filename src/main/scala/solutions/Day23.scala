package com.adventofcode
package solutions

import common.{Day, Input}

class Day23 extends Day {
  override val input: Input = new Input(23)

  override def solvePart1(input: String): Unit = {
    val routes = parseInput(input)
    val keys = routes.map(_._1)
    val groups = keys
      .flatMap(rt => {
        val connected = routes.filter(_._1 == rt).map(_._2)
        val pairs =
          keys
            .intersect(connected)
            .flatMap(k => connected.map(c => (c, k)))
            .intersect(routes)
        pairs.map(p => Set(rt, p._1, p._2))
      })
      .filter(s => s.exists(_.startsWith("t")))
    val count = groups.size
    println(f"Found $count groups")
  }

  override def solvePart2(input: String): Unit = {
    val routes = parseInput(input)
    val connected = routes.groupBy(r => r._1).map(p => (p._1, p._2.map(_._2)))
    val group = connected.keys.toList.sorted
      .map(k => findLargestGroup(routes, connected, Set(k)))
      .maxBy(_.size)
    val password = group.toList.sorted.mkString(",")
    println(f"Password is $password")
  }

  private def findLargestGroup(
      routes: Set[(String, String)],
      connected: Map[String, Set[String]],
      current: Set[String]
  ): Set[String] = {
    val expand = connected
      .getOrElse(current.min, Set.empty)
      .filter(_ > current.max)
      .filter(item => current.forall(curr => routes.contains((item, curr))))
    if (expand.isEmpty) {
      current
    } else {
      expand.toList
        .map(next => current ++ Set(next))
        .map(findLargestGroup(routes, connected, _))
        .maxBy(_.size)
    }
  }

  private def parseInput(input: String) = {
    getLines(input)
      .map(line => line.split('-'))
      .map(l => (l(0), l(1)))
      .flatMap(p => Seq(p, (p._2, p._1)))
      .toSet
  }
}
