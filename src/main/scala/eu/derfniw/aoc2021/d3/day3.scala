package eu.derfniw.aoc2021.d3

import scala.io.Source;

private def parseInput(in: Source) = {
  in.getLines().map(_.map(_.toString.toInt).reverse).toSeq
}

private def bitCounts(binNums: Seq[Seq[Int]]): Seq[Int] =
  binNums.foldLeft(List.fill(binNums.head.length)(0)) { (counts, nums) =>
    counts.zip(nums).map((count, num) => count + num)
  }

def exercise1(in: Source): Int = {
  val input       = parseInput(in)
  val inputLength = input.length
  val counts      = bitCounts(input)

  val gamma = counts.zipWithIndex.map { (num, idx) =>
    if (num > inputLength / 2) 1 * math.pow(2, idx).toInt
    else 0
  }.sum

  val epsilon = counts.zipWithIndex.map { (num, idx) =>
    if (num < inputLength / 2) 1 * math.pow(2, idx).toInt
    else 0
  }.sum

  gamma * epsilon
}

def exercise2(in: Source): Int = {
  val input  = parseInput(in)
  val counts = bitCounts(input)

  val ox = List
    .range(0, input.head.length)
    .reverse
    .foldLeft((input, counts)) {
      case ((i, c), _) if i.length == 1 => (i, c)
      case ((i, c), idx) =>
        val mostCommon = if (c(idx) < i.length / 2f) 0 else 1
        val newNums    = i.filter(_(idx) == mostCommon)
        (newNums, bitCounts(newNums))
    }
    ._1
    .head
    .zipWithIndex
    .map((n, i) => n * math.pow(2, i).toInt)
    .sum

  val co2 = List
    .range(0, input.head.length)
    .reverse
    .foldLeft((input, counts)) {
      case ((i, c), _) if i.length == 1 => (i, c)
      case ((i, c), idx) =>
        val leastCommon = if (c(idx) < i.length / 2f) 1 else 0
        val newNums     = i.filter(_(idx) == leastCommon)
        (newNums, bitCounts(newNums))
    }
    ._1
    .head
    .zipWithIndex
    .map((n, i) => n * math.pow(2, i).toInt)
    .sum

  ox * co2
}

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d03.txt")

@main
def run_3_1(): Unit = println(exercise1(input))

@main
def run_3_2(): Unit = println(exercise2(input))
