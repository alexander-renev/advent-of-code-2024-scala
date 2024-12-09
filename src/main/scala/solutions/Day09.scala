package com.adventofcode
package solutions

import common.{Day, Input}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class Day09 extends Day {
  private sealed trait FsElement {
    val isEmpty: Boolean;
    val size: Int;
    val index: Int;
  }
  private case class FileElement(index: Int, size: Int) extends FsElement {
    val isEmpty = false
  }

  private case class EmptyElement(size: Int) extends FsElement {
    val isEmpty = true
    val index = -1
  }

  override val input: Input = new Input(9)

  override def solvePart1(input: String): Unit = {
    val fs = parseInput(input)
    processFs(fs, 0, fs.length - 1)
    val checkSum = fs.zipWithIndex
      .filter(_._1 != -1)
      .map(p => BigInt(p._1) * BigInt(p._2))
      .sum
    println(f"Checksum is $checkSum")
  }

  @tailrec
  private def processFs(
      fs: Array[Int],
      startIndex: Int,
      endIndex: Int
  ): Unit = {
    if (startIndex < endIndex) {
      if (fs(startIndex) != -1) {
        processFs(fs, startIndex + 1, endIndex)
      } else if (fs(endIndex) == -1) {
        processFs(fs, startIndex, endIndex - 1)
      } else {
        fs(startIndex) = fs(endIndex)
        fs(endIndex) = -1
        processFs(fs, startIndex + 1, endIndex - 1)
      }
    }
  }

  override def solvePart2(input: String): Unit = {
    val fs = ListBuffer(parseInput2(input)*)
    val maxId = fs.map(_.index).max
    (maxId to 1 by -1).foreach(index => {
      val element = fs.reverseIterator.filter(_.index == index).next()
      val size = element.size
      val freeSpace = fs.find(p => p.isEmpty && p.size >= size)
      freeSpace.tapEach(free => {
        val originalIndex = fs.indexOf(element)
        val freeIndex = fs.indexOf(free)
        if (originalIndex > freeIndex) {
          fs.update(originalIndex, EmptyElement(element.size))
          fs.remove(freeIndex)
          fs.insert(freeIndex, element)
          if (free.size > size) {
            fs.insert(freeIndex + 1, EmptyElement(free.size - size))
          }
        }
      })
    })
    val checkSum = fs
      .flatMap(elem =>
        elem match {
          case FileElement(index, size) => List.fill(size)(Some(index))
          case EmptyElement(size)       => List.fill(size)(None)
        }
      )
      .zipWithIndex
      .map(p => BigInt(p._1.getOrElse(0)) * BigInt(p._2))
      .sum
    println(f"Checksum is $checkSum")
  }

  private def parseInput(input: String) = {
    input
      .map(_.toString.toInt)
      .zipWithIndex
      .flatMap(pair =>
        if (pair._2 % 2 == 0) List.fill(pair._1)(pair._2 / 2)
        else List.fill(pair._1)(-1)
      )
      .toArray
  }

  private def parseInput2(input: String): Array[FsElement] = {
    input
      .map(_.toString.toInt)
      .zipWithIndex
      .map(pair =>
        if (pair._2 % 2 == 0) FileElement(pair._2 / 2, pair._1)
        else EmptyElement(pair._1)
      )
      .filter(_.size > 0)
      .toArray
  }
}
