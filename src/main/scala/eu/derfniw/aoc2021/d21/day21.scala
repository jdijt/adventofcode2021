package eu.derfniw.aoc2021.d21

import eu.derfniw.aoc2021.printWithRuntime

import scala.annotation.tailrec
import scala.collection.mutable

def playerPositions(startPosition: Int): LazyList[Int] =
  LazyList.from(startPosition to 10).appendedAll(LazyList.continually(1 to 10).flatten)

def exercise1(p1Start: Int, p2Start: Int): Int =
  val diceThrows = LazyList.continually(1 to 100).flatten

  @tailrec
  def turn(
      player: Int,
      dice: LazyList[Int],
      position: Map[Int, LazyList[Int]],
      scores: Map[Int, Int],
      diceRolls: Int
  ): Int =
    val diceThrow      = dice.take(3).sum
    val playerPosition = position(player)(diceThrow)
    val newScore       = scores(player) + playerPosition
    val newPlayer      = if player == 1 then 2 else 1
    val newDiceRolls   = diceRolls + 3

    if newScore >= 1000 then newDiceRolls * scores(newPlayer)
    else
      turn(
        newPlayer,
        dice.drop(3),
        position + (player -> position(player).drop(diceThrow)),
        scores + (player   -> newScore),
        newDiceRolls
      )
    end if
  end turn

  turn(
    1,
    diceThrows,
    Map(1 -> playerPositions(p1Start), 2 -> playerPositions(p2Start)),
    Map.empty.withDefaultValue(0),
    0
  )
end exercise1

def exercise2(p1Start: Int, p2Start: Int): Long =
  val possibleDiceRolls = for
    one   <- 1 to 3
    two   <- 1 to 3
    three <- 1 to 3
  yield one + two + three

  val knownResults = mutable.Map[(Int, Int, Int, Int, Int), Map[Int, Long]]()
  // Regular recursion is safe because we know the depth is limited to 7 iterations (7 * 3 = 21).
  // Also, often won't even get there thanks to memoization.
  def turn(player: Int, position: Map[Int, LazyList[Int]], scores: Map[Int, Int]): Map[Int, Long] =
    val result = possibleDiceRolls.map { roll =>
      val playerPosition = position(player)(roll)
      val newScore       = scores(player) + playerPosition
      val newPlayer      = if player == 1 then 2 else 1

      if newScore >= 21 then Map(player -> 1L)
      else
        val newScores    = scores + (player   -> newScore)
        val newPositions = position + (player -> position(player).drop(roll))
        knownResults.getOrElseUpdate(
          (newPlayer, newPositions(1).head, newPositions(2).head, newScores(1), newScores(2)),
          turn(newPlayer, newPositions, newScores)
        )
    }
    result.flatMap(_.toList).groupMapReduce(_._1)(_._2)(_ + _)
  end turn
  turn(
    1,
    Map(1 -> playerPositions(p1Start), 2 -> playerPositions(p2Start)),
    Map.empty.withDefaultValue(0)
  ).values.max
end exercise2

@main
def run_20_1(): Unit = printWithRuntime(exercise1(7, 4))

@main
def run_20_2(): Unit = printWithRuntime(exercise2(7, 4))
