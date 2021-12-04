package eu.derfniw.aoc2021.d1

import eu.derfniw.aoc2021.*

import scala.io.Source

private lazy val input = Source.fromResource("exerciseInputs/input_d01.txt")

private def mapInput(in: Source) =
  in.getLines.map(_.toInt)

private def countIncreasingPairs(in: Iterator[Int]) =
  in.sliding(2).count { case prev +: next +: _ => prev < next }

def exercise1(in: Source): Int = countIncreasingPairs(mapInput(in))

def exercise2(in: Source): Int =
  val parsedInput = mapInput(in)
  val windowSums  = parsedInput.sliding(3).map(_.sum)

  countIncreasingPairs(windowSums)

@main
def run_1_1(): Unit =
  printWithRuntime(exercise1(input))

@main
def run_1_2(): Unit =
  printWithRuntime(exercise2(input))
