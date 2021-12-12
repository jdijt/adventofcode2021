package eu.derfniw.aoc2021.d12

import eu.derfniw.aoc2021.printWithRuntime

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

enum Cave(val id: String):
  case StartCave extends Cave("start")
  case BigCave(name: String) extends Cave(name)
  case SmallCave(name: String) extends Cave(name)
  case EndCave extends Cave("end")

object Cave:
  def apply(in: String): Cave = in match
    case "start"                  => StartCave
    case "end"                    => EndCave
    case s if s.forall(_.isUpper) => BigCave(s)
    case s                        => SmallCave(s)
end Cave

class CaveSystem(connections: Set[(Cave, Cave)]):
  private val adjacency: Map[Cave, Set[Cave]] =
    connections.flatMap { case (f, t) => Set((f, t), (t, f)) }.groupMap(_._1)(_._2)

  def adjacentCaves(cave: Cave): Set[Cave] =
    adjacency.getOrElse(cave, Set.empty)
end CaveSystem

private def parseInput(source: Source): CaveSystem =
  val pairs = source.getLines().map { case s"$from-$to" => (Cave(from), Cave(to)) }
  CaveSystem(pairs.toSet)

def exercise1(source: Source): Int =
  import Cave.*
  val caveSystem = parseInput(source)

  def dfs(current: Cave, visited: Set[Cave] = Set.empty): Int =
    def nextCaves = caveSystem.adjacentCaves(current).removedAll(visited).toList

    current match
      case EndCave                  => 1
      case SmallCave(_) | StartCave => nextCaves.map(c => dfs(c, visited + current)).sum
      case BigCave(_)               => nextCaves.map(c => dfs(c, visited)).sum
  end dfs

  dfs(Cave.StartCave)
end exercise1

def exercise2(source: Source): Int =
  import Cave.*
  val caveSystem = parseInput(source)

  def dfs(current: Cave, visited: Set[Cave] = Set.empty, smallCaveTwice: Boolean = false): Int =
    // helper defs:
    def nextCaves = caveSystem.adjacentCaves(current).removedAll(visited).toList
    def additionalSmallCaves =
      if smallCaveTwice then List.empty
      else (caveSystem.adjacentCaves(current).intersect(visited) - StartCave).toList

    current match
      case EndCave => 1
      case SmallCave(_) | StartCave =>
        nextCaves.map(c => dfs(c, visited + current, smallCaveTwice)).sum
          + additionalSmallCaves.map(c => dfs(c, visited + current, true)).sum
      case BigCave(_) =>
        nextCaves.map(c => dfs(c, visited, smallCaveTwice)).sum
          + additionalSmallCaves.map(c => dfs(c, visited, true)).sum
    end match
  end dfs

  dfs(Cave.StartCave)
end exercise2

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d12.txt")

@main
def run_12_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_12_2(): Unit = printWithRuntime(exercise2(input))
