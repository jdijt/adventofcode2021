package eu.derfniw.aoc2021.d16

import eu.derfniw.aoc2021.printWithRuntime

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

import java.math.BigInteger
import math.*

def exercise1(source: Source): Int =
  def sumVersions(current: Packet): Int = current match
    case LiteralValue(version, _)       => version
    case Operator(version, _, children) => version + children.map(sumVersions).sum
  sumVersions(source.parsed)

def exercise2(source: Source): BigInt =
  def calculateValue(current: Packet): BigInt = current match
    case LiteralValue(_, value) => value
    case Operator(_, typeId, children) =>
      val childValues = children.map(calculateValue)
      typeId match
        case 0 => childValues.sum
        case 1 => childValues.product
        case 2 => childValues.min
        case 3 => childValues.max
        //   4 would be the LiteralValue ;)
        case 5 => if childValues.head > childValues.last then BigInt(1) else BigInt(0)
        case 6 => if childValues.head < childValues.last then BigInt(1) else BigInt(0)
        case 7 => if childValues.head == childValues.last then BigInt(1) else BigInt(0)
  end calculateValue
  calculateValue(source.parsed)
end exercise2

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d16.txt")

@main
def run_16_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_16_2(): Unit = printWithRuntime(exercise2(input))
