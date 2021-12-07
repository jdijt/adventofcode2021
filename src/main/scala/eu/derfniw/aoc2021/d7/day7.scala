package eu.derfniw.aoc2021.d7

import eu.derfniw.aoc2021.*

import scala.io.Source

private def parseInput(in: Source): Seq[Int] =
  in.getLines().next().split(",").map(_.toInt)

private def minFuelCost(
    crabPositions: Seq[Int],
    costFunction: Int => Int
): Int =
  // Don't go below or above min/max pos.
  // This will by definition be worse:
  // sticking to `min` or `max` will be guaranteed to be one step less
  // And therefore less fuel.
  val minPosition = crabPositions.min
  val maxPosition = crabPositions.max

  (minPosition to maxPosition).map { targetPosition =>
    crabPositions
      .map(initialPosition => costFunction(math.abs(initialPosition - targetPosition)))
      .sum
  }.min
end minFuelCost

def exercise1(source: Source): Int =
  val initialPositions = parseInput(source)
  minFuelCost(initialPositions, identity)

def exercise2(source: Source): Int =
  val initialPositions = parseInput(source)
  // The nth triangular number (1 + 2 + 3 + 4...etc..)
  def costFunction(steps: Int) = (steps * (steps + 1)) / 2
  minFuelCost(initialPositions, costFunction)

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d07.txt")

@main
def run_7_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_7_2(): Unit = printWithRuntime(exercise2(input))
