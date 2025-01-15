package com.adventofcode
package solutions

import scala.collection.mutable
import common.{Day, Input}

class Day24 extends Day {
  override val input: Input = new Input(24)

  private sealed trait Expression {
    def describe(): String
    def describeSafe(): String
  }

  private case class Variable(name: String) extends Expression {
    override def describe(): String = name
    override def describeSafe(): String = name

    override def toString: String = describe()
  }

  private object Variable {
    def withName(prefix: String, value: Int): Variable = {
      Variable(f"$prefix$value%02d")
    }

    def format(prefix: String, value: Int): String = {
      f"$prefix$value%02d"
    }
  }

  private case class BinaryExpression(
      op1: Expression,
      op2: Expression,
      operator: String
  ) extends Expression {

    override def describe(): String = {
      val val1 = op1.describeSafe()
      val val2 = op2.describeSafe()
      Array(val1, val2).sorted.mkString(f" $operator ")
    }
    override def describeSafe(): String = "(" + describe() + ")"
    override def toString: String = describe()
  }

  private object BinaryExpression {
    def create(
        op1: Expression,
        op2: Expression,
        operator: String
    ): BinaryExpression = {
      val val1 = op1.describe()
      val val2 = op2.describe()
      if (val1 < val2) {
        BinaryExpression(op1, op2, operator)
      } else {
        BinaryExpression(op2, op1, operator)
      }
    }
  }

  private case class Operation(
      op1: String,
      op2: String,
      operation: String,
      target: String
  )

  override def solvePart1(input: String): Unit = {
    val (data, operations) = parseInput(input)
    val calculated = mutable.Map[String, Int]()
    data.foreach(p => calculated(p._1) = p._2)
    val allKeys = operations.flatMap(o => Seq(o._2, o._3, o._4)).distinct
    val keysCount = allKeys.length
    val remainingOperations = mutable.ListBuffer(operations *)

    while (calculated.size < keysCount) {
      val operationToProcess = remainingOperations.filter(op =>
        calculated.contains(op._2) && calculated.contains(op._3)
      )
      operationToProcess.foreach(op => {
        val result = op._1(calculated(op._2), calculated(op._3))
        calculated(op._4) = result
      })
      remainingOperations --= operationToProcess
    }

    val number = calculated.keys
      .filter(_.startsWith("z"))
      .toList
      .sorted
      .reverse
      .map(k => calculated(k))
      .mkString
    val parsed = BigInt(number, 2)
    println(f"Number is $parsed")
  }

  override def solvePart2(input: String): Unit = {
    val (_, operations) = parseInput(input)
    val mapping = mutable.Map[String, Expression]()
    mapping("z00") = BinaryExpression.create(
      Variable.withName("x", 0),
      Variable.withName("y", 0),
      "XOR"
    )

    var counter = 1
    def next(): String = {
      val result = f"new$counter"
      counter += 1
      result
    }
    var other = next()
    mapping(other) = BinaryExpression.create(
      Variable.withName("x", 0),
      Variable.withName("y", 0),
      "AND"
    )
    (1 to 44).foreach(num => {
      val strNum = f"$num%02d"
      val z = f"z$strNum"
      val xor = next()
      mapping(xor) = BinaryExpression.create(
        Variable.withName("x", num),
        Variable.withName("y", num),
        "XOR"
      )
      val and = next()
      mapping(and) = BinaryExpression.create(
        Variable.withName("x", num),
        Variable.withName("y", num),
        "AND"
      )
      mapping(z) = BinaryExpression.create(mapping(other), mapping(xor), "XOR")
      val other1 = next()
      mapping(other1) =
        BinaryExpression.create(mapping(xor), mapping(other), "AND")
      other = next()
      mapping(other) =
        BinaryExpression.create(mapping(and), mapping(other1), "OR")
    })

    var (sourceMapping, reverseMapping) = processOperations(operations)
    val changes = mutable.ListBuffer[String]()

    (0 to 44).foreach(num => {
      val name = Variable.format("z", num)
      val target = mapping(name)
      val source = sourceMapping(name)
      if (target.describe() != source.describe()) {
        var (currTgt, currSrc) = (target, source)
        def moveNext(): Boolean = {
          (currTgt, currSrc) match {
            case (_: Variable, _: Variable) => false
            case (op1: BinaryExpression, op2: BinaryExpression) =>
              op1.operator == op2.operator && (op1.op1.describe() == op2.op1
                .describe() || op1.op2.describe() == op2.op2.describe())
            case _ => throw RuntimeException("Invalid combination")
          }
        }
        while (moveNext()) {
          val exp1 = currTgt.asInstanceOf[BinaryExpression]
          val exp2 = currSrc.asInstanceOf[BinaryExpression]
          if (exp1.op1.describe() == exp2.op1.describe()) {
            currTgt = exp1.op2
            currSrc = exp2.op2
          } else {
            currTgt = exp1.op1
            currSrc = exp2.op1
          }
        }

        val found = reverseMapping(currTgt.describe())
        val found2 = reverseMapping(currSrc.describe())

        val ind1 = operations.indexWhere(p => p._4 == found)
        val op1 = operations(ind1)
        val ind2 = operations.indexWhere(p => p._4 == found2)
        val op2 = operations(ind2)

        operations(ind1) = (op1._1, op1._2, op1._3, found2, op1._5)
        operations(ind2) = (op2._1, op2._2, op2._3, found, op2._5)

        val res = processOperations(operations)
        sourceMapping = res._1
        reverseMapping = res._2

        changes.appendAll(Seq(found, found2))

        println(f"Found changes $found $found2")
      }
    })

    println(changes.sorted.mkString(","))
  }

  private def processOperations(
      operations: Array[((Int, Int) => Int, String, String, String, String)]
  ) = {
    val sourceMapping = mutable.Map[String, Expression]()
    (0 to 44).foreach(num => {
      val x = Variable.withName("x", num)
      val y = Variable.withName("y", num)
      sourceMapping(x.name) = x
      sourceMapping(y.name) = y
    })
    val variableCount = sourceMapping.size
    while (sourceMapping.size < operations.length + variableCount) {
      operations
        .filter(op =>
          sourceMapping.contains(op._2) && sourceMapping.contains(op._3)
        )
        .foreach(op => {
          sourceMapping(op._4) = BinaryExpression
            .create(sourceMapping(op._2), sourceMapping(op._3), op._5)
        })
    }

    val reverseMapping = sourceMapping.map(p => (p._2.describe(), p._1)).toMap
    (sourceMapping, reverseMapping)
  }

  private def parseInput(input: String) = {
    val groups = parseLineGroups(input)
    val data = groups(0)
      .map(grp => grp.split(' '))
      .map(grp => {
        (grp(0).slice(0, grp(0).length - 1), grp(1).toInt)
      })
      .toMap

    def xor(a: Int, b: Int) = if (a == b) 0 else 1
    def and(a: Int, b: Int) = if (a == 1 && b == 1) 1 else 0
    def or(a: Int, b: Int) = if (a == 0 && b == 0) 0 else 1

    val operations = groups(1).map(grp => {
      val parts = grp.split(' ')
      val op1 = parts(0)
      val op2 = parts(2)
      val op = parts(1) match {
        case "XOR" => xor
        case "AND" => and
        case "OR"  => or
        case _     => throw RuntimeException(f"Unknown operation ${parts(1)}")
      }
      val target = parts(4)
      (op, op1, op2, target, parts(1))
    })

    (data, operations)
  }
}
