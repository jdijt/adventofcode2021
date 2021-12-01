package eu.derfniw.aoc2021.d1

import scala.io.Source

private lazy val input = Source.fromResource("exerciseInputs/input_ex1.txt")

private def mapInput(in: Source) =
  in.getLines.filter(_.matches("""[0-9]+""")).map(_.toInt).toList

private def countIncreasingPairs(in: List[Int]) =
  in.zip(in.tail).map((prev, next) => if (prev < next) 1 else 0).sum

def exercise1(in: Source): Int = countIncreasingPairs(mapInput(in))

def exercise2(in: Source): Int = {
  val parsedInput = mapInput(in)
  val windowSums  = parsedInput.sliding(3).map(_.sum).toList

  countIncreasingPairs(windowSums)
}

@main
def run_1_1(): Unit = {
  println(exercise1(input))
}

@main
def run_1_2(): Unit = {
  println(exercise2(input))
}
