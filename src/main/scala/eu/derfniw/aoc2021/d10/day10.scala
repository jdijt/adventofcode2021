package eu.derfniw.aoc2021.d10

import eu.derfniw.aoc2021.printWithRuntime

import scala.annotation.tailrec
import scala.io.Source

extension (source: Source) def parse: List[List[Char]] = source.getLines().map(_.toList).toList

extension (c: Char)
  def isOpeningBracket = "[({<".contains(c)
  def isClosingBracket = "])}>".contains(c)

  def oppositeBracket = c match
    case '(' => ')'
    case '[' => ']'
    case '{' => '}'
    case '<' => '>'
    case _   => throw new RuntimeException(s"No opposite bracket for $c")

  def getScoreIllegal = c match
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
    case _   => throw new RuntimeException(s"No score for $c")

  def getScoreAutoComplete = c match
    case ')' => 1
    case ']' => 2
    case '}' => 3
    case '>' => 4
    case _   => throw new RuntimeException(s"No score for $c")
end extension

def exercise1(source: Source): Int =
  val lines = source.parse

  @tailrec
  def checkLine(line: List[Char], bracketStack: List[Char] = Nil): Option[Char] =
    line match
      case Nil                           => None
      case c :: cs if c.isOpeningBracket => checkLine(cs, c.oppositeBracket :: bracketStack)
      case c :: cs if c.isClosingBracket =>
        bracketStack match
          case b :: bs if c == b => checkLine(cs, bs) // Valid: Closing bracket
          case _ => Some(c) // Mismatch! Expected b, found c.

  lines.flatMap(l => checkLine(l, Nil)).map(_.getScoreIllegal).sum
end exercise1

def exercise2(source: Source): Long =
  val lines = source.parse

  @tailrec
  def missingSuffix(line: List[Char], bracketStack: List[Char] = Nil): Option[List[Char]] =
    line match
      case Nil                           => Some(bracketStack) // leftover brackets that we expected
      case c :: cs if c.isOpeningBracket => missingSuffix(cs, c.oppositeBracket :: bracketStack)
      case c :: cs if c.isClosingBracket =>
        bracketStack match
          case b :: bs if c == b => missingSuffix(cs, bs)
          case _ => None // This line is illegal, no suffix to be computed.

  val scores = lines
    .flatMap(l => missingSuffix(l))
    .map(_.foldLeft(0L)((score, c) => (5 * score) + c.getScoreAutoComplete))

  scores.sorted.apply((scores.length - 1) / 2)
end exercise2

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d10.txt")

@main
def run_10_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_10_2(): Unit = printWithRuntime(exercise2(input))
