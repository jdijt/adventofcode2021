package eu.derfniw.aoc2021.d14

import eu.derfniw.aoc2021.printWithRuntime

import scala.annotation.tailrec
import scala.io.Source

type PairMapping = PartialFunction[String, Char]

class PolymerChain private (last: Char, counts: Map[String, BigInt]):

  @tailrec
  final def simulateGrowth(count: Int, mapping: PairMapping): PolymerChain =
    if count <= 0 then this
    else this.withMapping(mapping).simulateGrowth(count - 1, mapping)

  def withMapping(pairMapping: PairMapping): PolymerChain =
    val newCounts = counts.toSeq
      .flatMap {
        case (p, c) if pairMapping.isDefinedAt(p) =>
          Seq(s"${p.head}${pairMapping(p)}" -> c, s"${pairMapping(p)}${p.last}" -> c)
        case other => Seq(other)
      }
      .groupMapReduce(_._1)(_._2)(_ + _)
    PolymerChain(last, newCounts)
  end withMapping

  def elementCounts: Map[Char, BigInt] =
    val baseCounts = counts.groupMapReduce(_._1.head)(_._2)(_ + _)
    baseCounts.updated(last, baseCounts.getOrElse(last, BigInt(0)) + 1)
end PolymerChain

object PolymerChain:
  def fromLine(line: String): PolymerChain =
    val counts = line.sliding(2).toSeq.groupMapReduce(identity)(_ => BigInt(1))(_ + _)
    PolymerChain(line.last, counts)

end PolymerChain

extension (source: Source)
  def parsed: (PolymerChain, PairMapping) =
    val lines           = source.getLines()
    val initialPolymers = PolymerChain.fromLine(lines.next())
    lines.next()
    val mappings = lines.map {
      case s"$source -> $dest" if dest.length == 1 => source -> dest.head
    }.toMap
    (initialPolymers, mappings)

def exercise1(source: Source): BigInt =
  val (polymer, mappings) = source.parsed
  val elementCounts       = polymer.simulateGrowth(10, mappings).elementCounts
  elementCounts.values.max - elementCounts.values.min
end exercise1

def exercise2(source: Source): BigInt =
  val (polymer, mappings) = source.parsed
  val elementCounts       = polymer.simulateGrowth(40, mappings).elementCounts
  elementCounts.values.max - elementCounts.values.min
end exercise2

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d14.txt")

@main
def run_13_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_13_2(): Unit = printWithRuntime(exercise2(input))
