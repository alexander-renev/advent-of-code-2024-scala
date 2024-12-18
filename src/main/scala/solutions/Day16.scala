package com.adventofcode
package solutions

import scala.jdk.StreamConverters.*
import scala.collection.mutable
import common.{Day, Input}

class Day16 extends Day {
  override val input: Input = new Input(16)

  override def solvePart1(input: String): Unit = {
    val minPrice = getMinPrice(input)
    println(f"Min price is $minPrice")
  }

  override def solvePart2(input: String): Unit = {
    val minPrice = getMinPrice(input)
    val routes = getMinRoutes(input)
    val elements = routes
      .filter(_._1 == minPrice)
      .flatMap(_._2)
      .flatten
      .map(_._1)
      .distinct
      .length
    println(f"Elements count $elements")
  }

  private def getMinRoutes(input: String) = {
    val rawBoard = parseInput(input)
    val start = rawBoard.find(_._2 == 'S').get._1
    val finish = rawBoard.find(_._2 == 'E').get._1
    val board = rawBoard.updated(start, '.').updated(finish, '.')
    val routes = mutable.Map[(Position, Position), Option[
      (Int, List[List[(Position, Position)]])
    ]]()
    val directions =
      Seq(-1, 1).flatMap(d => Seq(Position(d, 0), Position(0, d))).toList
    board
      .filter(_._2 == '.')
      .keys
      .flatMap(p => directions.map((p, _)))
      .foreach(routes(_) = None)
    routes((start, Position(1, 0))) =
      Some(0, List(List((start, Position(1, 0)))))
    val checked = mutable.Set[(Position, Position)]()
    while (checked.size < routes.size) {
      val toCheck = routes
        .filter(p => !checked.contains(p._1))
        .toList
        .minBy(p => p._2.map(_._1).getOrElse(Int.MaxValue))
      val position = toCheck._1._1
      val direction = toCheck._1._2
      val leadingRoutes = toCheck._2.get._2
      val price = toCheck._2.get._1
      val newDirections =
        if (direction.x == 0) Array(Position(1, 0), Position(-1, 0))
        else Array(Position(0, 1), Position(0, -1))
      List(
        (position + direction, direction, price + 1),
        (position, newDirections(0), price + 1000),
        (position, newDirections(1), price + 1000)
      )
        .filter(p => routes.contains((p._1, p._2)))
        .foreach(newRoute => {
          val (newPosition, newDirection, newPrice) = newRoute
          val routeKey = (newPosition, newDirection)
          val currentPrice = routes(routeKey)
          if (currentPrice.isEmpty || currentPrice.get._1 > newPrice) {
            val newRoutes = leadingRoutes.map(lr => lr.appended(routeKey))
            routes(routeKey) = Some((newPrice, newRoutes))
          } else if (currentPrice.get._1 == newPrice) {
            // We got another route
            val newRoutes = leadingRoutes
              .map(lr => lr.appended(routeKey)) ++ currentPrice.get._2
            routes(routeKey) = Some((newPrice, newRoutes))
          }
        })
      checked.add(toCheck._1)
    }
    routes.filter(p => p._1._1 == finish).values.map(_.get).toList
  }

  private def getMinPrice(input: String) = {
    val rawBoard = parseInput(input)
    val start = rawBoard.find(_._2 == 'S').get._1
    val finish = rawBoard.find(_._2 == 'E').get._1
    val board = rawBoard.updated(start, '.').updated(finish, '.')
    val routes = mutable.Map[(Position, Position), Option[
      Int
    ]]()
    val directions =
      Seq(-1, 1).flatMap(d => Seq(Position(d, 0), Position(0, d))).toList
    board
      .filter(_._2 == '.')
      .keys
      .flatMap(p => directions.map((p, _)))
      .foreach(routes(_) = None)
    routes((start, Position(1, 0))) = Some(0)
    val checked = mutable.Set[(Position, Position)]()
    while (checked.size < routes.size) {
      val toCheck = routes
        .filter(p => !checked.contains(p._1))
        .toList
        .minBy(p => p._2.getOrElse(Int.MaxValue))
      val position = toCheck._1._1
      val direction = toCheck._1._2
      val price = toCheck._2.get
      val newDirections =
        if (direction.x == 0) Array(Position(1, 0), Position(-1, 0))
        else Array(Position(0, 1), Position(0, -1))
      List(
        (position + direction, direction, price + 1),
        (position, newDirections(0), price + 1000),
        (position, newDirections(1), price + 1000)
      )
        .filter(p => routes.contains((p._1, p._2)))
        .foreach(newRoute => {
          val (newPosition, newDirection, newPrice) = newRoute
          val routeKey = (newPosition, newDirection)
          val currentPrice = routes(routeKey)
          if (currentPrice.isEmpty || currentPrice.get > newPrice) {
            routes(routeKey) = Some(newPrice)
          }
        })
      checked.add(toCheck._1)
    }
    routes.filter(p => p._1._1 == finish).values.map(_.get).toList.min
  }

  private def parseInput(input: String) = {
    input
      .lines()
      .toScala(Array)
      .map(line => line.zipWithIndex)
      .zipWithIndex
      .flatMap(v => v._1.map(n => (Position(n._2, v._2), n._1)))
      .toMap
  }
}
