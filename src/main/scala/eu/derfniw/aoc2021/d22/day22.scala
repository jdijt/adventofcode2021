package eu.derfniw.aoc2021.d22

import eu.derfniw.aoc2021.printWithRuntime

import scala.io.Source

def exercise1(source: Source): Int =
  val instrs = InstructionParser.parse(source)
  val xRange = -50 to 50
  val yRange = -50 to 50
  val zRange = -50 to 50

  val pointsToCheck = for x <- xRange; y <- yRange; z <- xRange yield Point(x, y, z)

  pointsToCheck
    .flatMap(p =>
      instrs.filter(_.appliesTo(p)).lastOption.map {
        case On(_, _, _)  => 1
        case Off(_, _, _) => 0
      }
    )
    .sum

end exercise1

def exercise2(source: Source): Int =
  ???
end exercise2

lazy val input: Source = Source.fromResource("exerciseInputs/input_d22.txt")

@main
def run_22_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_22_2(): Unit = printWithRuntime(exercise2(input))
