package eu.derfniw.aoc2021.d18

import eu.derfniw.aoc2021.printWithRuntime

import scala.io.Source

extension (s: Source) def getNumbers = s.getLines().map(SnailNumber.fromString)

def exercise1(source: Source): Long =
  source.getNumbers
    .reduce((l, r) => l.add(r))
    .magnitude

def exercise2(source: Source): Long =
  source.getNumbers.toSeq
    .combinations(2)
    .flatMap { case l +: r +: _ => Seq(l.add(r).magnitude, r.add(l).magnitude) }
    .max

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d18.txt")

@main
def run_18_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_18_2(): Unit = printWithRuntime(exercise2(input))
