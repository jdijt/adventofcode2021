package eu.derfniw.aoc2021.d8

import eu.derfniw.aoc2021.printWithRuntime

import scala.io.Source

class Line(in: String):
  private val notes  = in.substring(0, in.indexOf(" | ")).split(" ").map(_.toSet)
  private val output = in.substring(in.indexOf(" | ") + 3).split(" ")

  // Base patterns
  private val onePattern   = notes.find(_.size == 2).get
  private val sevenPattern = notes.find(_.size == 3).get
  private val fourPattern  = notes.find(_.size == 4).get
  private val eigthPattern = notes.find(_.size == 7).get
  // 9 is the only number with length 6 that overlaps with 4
  private val ninePattern = notes
    .filter(_.size == 6)
    .find { c =>
      fourPattern.removedAll(c).isEmpty
    }
    .get
  // 6 is the only number with length 6 that doesn't overlap with 7.
  private val sixPattern = notes
    .filter(_.size == 6)
    .find(c => sevenPattern.removedAll(c).size == 1)
    .get
  // 0 is not 9 or 6
  private val zeroPattern = notes
    .filter(_.size == 6)
    .find(c => c != ninePattern && c != sixPattern)
    .get
  // Five can be constructed from 8, six and nine.
  private val fivePattern =
    eigthPattern.intersect(sixPattern).intersect(ninePattern)
  // Two is the only one of the 5 length numbers that has the two values that 5 doesn't.
  private val twoPattern = notes
    .filter(_.size == 5)
    .find(_.count(eigthPattern.removedAll(fivePattern)) == 2)
    .get
  // Three isn't five or two
  private val threePattern = notes
    .filter(_.size == 5)
    .find(c => c != fivePattern && c != twoPattern)
    .get

  private def getNumValue(value: String): Int =
    val set = value.toSet
    if set == zeroPattern then 0
    else if set == onePattern then 1
    else if set == twoPattern then 2
    else if set == threePattern then 3
    else if set == fourPattern then 4
    else if set == fivePattern then 5
    else if set == sixPattern then 6
    else if set == sevenPattern then 7
    else if set == eigthPattern then 8
    else if set == ninePattern then 9
    else throw new RuntimeException(s"Bad pattern! $value $set")
    end if
  end getNumValue

  def getValue: Int =
    output.reverse.zipWithIndex.map((v, i) => getNumValue(v) * math.pow(10, i).toInt).sum

end Line

//deduced patterns:

def exercise1(source: Source): Int = source
  .getLines()
  .flatMap(s => s.substring(s.indexOf(" | ") + 3).split(""" """))
  .count(s => s.length == 2 || s.length == 3 || s.length == 4 || s.length == 7)

def exercise2(source: Source): Int =
  source.getLines().map(l => new Line(l)).map(_.getValue).sum

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d08.txt")

@main
def run_8_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_8_2(): Unit = printWithRuntime(exercise2(input))