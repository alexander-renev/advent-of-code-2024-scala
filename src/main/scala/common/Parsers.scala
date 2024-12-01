package com.adventofcode
package common

import scala.util.parsing.combinator.RegexParsers

trait Parsers extends RegexParsers {
  def parseInput[T](input: String, parser: Parser[T]): T = {
    parse(parser, input) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => throw new RuntimeException(s"FAILURE: $msg")
      case Error(msg, _) => throw new RuntimeException(s"ERROR: $msg")
    }
  }
}
