package eu.derfniw.aoc2021.d6

import eu.derfniw.aoc2021.*

import scala.io.Source

case class FishPop(day: Int, count: Long)

private def parseInput(in: Source): Seq[Int] =
  in.getLines().next().split(",").map(_.toInt)

// Deduplicates lists of fish population to one FishPop per day
extension (fishPops: Iterable[FishPop])
  def deduplicate: Iterable[FishPop] =
    fishPops.groupBy(_.day).map((d, pops) => FishPop(d, pops.map(_.count).sum))

private def simulatePopulation(in: Source, iterations: Int): Long =
  // Seq[(days, count)]
  val initial =
    parseInput(in)
      .map(d => FishPop(d, 1))
      .deduplicate

  (0 until iterations)
    .foldLeft(initial) { (state, _) =>
      state.flatMap {
        case FishPop(0, n) => Seq(FishPop(6, n), FishPop(8, n))
        case FishPop(d, n) => Seq(FishPop(d - 1, n))
      }.deduplicate
    }
    .map(_.count)
    .sum
end simulatePopulation

def exercise1(source: Source): Long = simulatePopulation(source, 80)

def exercise2(source: Source): Long = simulatePopulation(source, 256)

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d06.txt")

@main
def run_6_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_6_2(): Unit = printWithRuntime(exercise2(input))
