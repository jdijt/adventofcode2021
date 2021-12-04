package eu.derfniw.aoc2021.d3

import scala.io.Source

private def parseInput(in: Source): BitValues = {
  val lines    = in.getLines().toSeq
  val bitWidth = lines.head.length
  BitValues(lines.map(str => Integer.valueOf(str, 2)), bitWidth)
}

class BitValues(vals: Seq[Int], val bitWidth: Int) {

  private val bitMasks = (0 until bitWidth).map(1 << _)

  val length: Int = vals.length
  val head: Int   = vals.head

  val oneCounts: Seq[Int] = bitMasks.map { mask =>
    vals.map(_ & mask).count(_ > 0)
  }

  val zeroCounts: Seq[Int] = bitMasks.map { mask =>
    vals.map(_ & mask).count(_ == 0)
  }

  def keepOnes(bitIndex: Int): BitValues =
    BitValues(vals.filter(v => (v & bitMasks(bitIndex)) > 0), bitWidth)

  def keepZeroes(bitIndex: Int): BitValues =
    BitValues(vals.filter(v => (v & bitMasks(bitIndex)) == 0), bitWidth)
}

def exercise1(in: Source): Int = {
  val input    = parseInput(in)
  val combined = input.oneCounts.zip(input.zeroCounts).zipWithIndex
  val gamma = combined.map { case ((oneCount, zeroCount), idx) =>
    (if (oneCount > zeroCount) 1 else 0) * math.pow(2, idx).toInt
  }.sum

  val epsilon = combined.map { case ((oneCount, zeroCount), idx) =>
    (if (oneCount < zeroCount) 1 else 0) * math.pow(2, idx).toInt
  }.sum

  gamma * epsilon
}

def exercise2(in: Source): Int = {
  val input          = parseInput(in)
  val indexesToCheck = (0 until input.bitWidth).reverse

  val ox = indexesToCheck
    .foldLeft(input) {
      case (i, _) if i.length == 1 => i
      case (i, idx) =>
        if (i.oneCounts(idx) >= i.zeroCounts(idx)) i.keepOnes(idx)
        else i.keepZeroes(idx)
    }
    .head

  val co2 = indexesToCheck
    .foldLeft(input) {
      case (i, _) if i.length == 1 => i
      case (i, idx) =>
        if (i.oneCounts(idx) < i.zeroCounts(idx)) i.keepOnes(idx)
        else i.keepZeroes(idx)
    }
    .head

  ox * co2
}

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d03.txt")

@main
def run_3_1(): Unit = println(exercise1(input))

@main
def run_3_2(): Unit = println(exercise2(input))
