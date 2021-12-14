package eu.derfniw.aoc2021.d14

import eu.derfniw.aoc2021.printWithRuntime

import scala.annotation.tailrec
import scala.io.Source

type PairMapping = PartialFunction[String, Char]

extension (source: Source)
  def parsed: (String, PairMapping) =
    val lines           = source.getLines()
    val initialPolimers = lines.next()
    lines.next() // skip empty line
    val mappings = lines.map {
      case s"$source -> $dest" if dest.length == 1 => source -> dest.head
    }.toMap
    (initialPolimers, mappings)

def exercise1(source: Source): Int =
  val (polymer, mappings) = source.parsed
  val result = (0 until 10).foldLeft(polymer)((p, _) =>
    p.sliding(2)
      .flatMap {
        case pair if mappings.isDefinedAt(pair) => s"${pair.head}${mappings(pair)}"
        case pair                               => s"${pair.head}"
      }
      .mkString + p.last.toString
  )
  val characterCounts = result.groupMapReduce(identity)(_ => 1)(_ + _)
  characterCounts.values.max - characterCounts.values.min
end exercise1

def exercise2(source: Source): Long =
  val (polymer, mappings) = source.parsed
  val result = (0 until 40).foldLeft(polymer)((p, c) =>
    println(c)
    p.sliding(2)
      .flatMap {
        case pair if mappings.isDefinedAt(pair) => s"${pair.head}${mappings(pair)}"
        case pair                               => s"${pair.head}"
      }
      .mkString + p.last.toString
  )
  val characterCounts = result.groupMapReduce(identity)(_ => 1L)(_ + _)
  characterCounts.values.max - characterCounts.values.min
end exercise2

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d14.txt")

@main
def run_13_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_13_2(): Unit = printWithRuntime(exercise2(input))
