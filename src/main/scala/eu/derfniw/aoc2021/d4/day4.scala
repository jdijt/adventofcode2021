package eu.derfniw.aoc2021.d4

import eu.derfniw.aoc2021.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

enum Field:
  case Marked extends Field
  case UnMarked(v: Int) extends Field

case class Board(values: Seq[Seq[Field]]):
  import Field.*

  val isWinner: Boolean =
    val hasWinningRow    = values.exists(_.forall(_ == Marked))
    val columns          = (0 until 5).map(col => (0 until 5).map(row => values(row)(col)))
    val hasWinningColumn = columns.exists(_.forall(_ == Marked))
    hasWinningColumn || hasWinningRow

  def markValue(value: Int): Board =
    val newValues = values.map(_.map {
      case UnMarked(v) if v == value => Marked
      case other                     => other
    })
    Board(newValues)

  def sumUnmarked: Int = values.flatten.collect { case UnMarked(v) => v }.sum
end Board

// First line: Input, then newline seperated boards of 5x5 int
private def parseInput(in: Source): (List[Int], List[Board]) =
  import Field.*
  val lines     = in.getLines()
  val inputNums = lines.next().split(",").map(_.toInt).toList

  val boardsCollector = mutable.ArrayDeque[Board]()

  while lines.hasNext do
    lines.next() // Skip empty line
    val fields =
      (0 until 5).map(_ => lines.next().strip().split("""\s+""").map(v => UnMarked(v.toInt)).toSeq)
    boardsCollector.addOne(Board(fields))
  end while

  (inputNums, boardsCollector.toList)
end parseInput

def exercise1(in: Source): Int =
  val (inputNumbers, boards) = parseInput(in)

  @tailrec
  def playGame(input: List[Int], state: List[Board]): (Int, Board) =
    input match
      case Nil => throw new RuntimeException("Reached end of input without winning board")
      case in :: rest =>
        val newState = state.map(_.markValue(in))
        newState.find(_.isWinner) match
          case Some(winner) => (in, winner)
          case _            => playGame(rest, newState)
  end playGame

  val (winningNumber, winningBoard) = playGame(inputNumbers, boards)
  winningNumber * winningBoard.sumUnmarked
end exercise1

def exercise2(in: Source): Int =
  val (inputNumbers, boards) = parseInput(in)

  @tailrec
  def playGame(input: List[Int], state: List[Board], winners: Seq[(Int, Board)]): (Int, Board) =
    if state.isEmpty then winners.last
    else
      input match
        case Nil => winners.last // Not all boards have to win!
        case in :: rest =>
          val newState   = state.map(_.markValue(in))
          val newWinners = winners.appendedAll(newState.filter(_.isWinner).map((in, _)))
          val losers     = newState.filterNot(_.isWinner)
          playGame(rest, losers, newWinners)
  end playGame

  val (winningNumber, winningBoard) = playGame(inputNumbers, boards, List())
  winningNumber * winningBoard.sumUnmarked
end exercise2

private lazy val input: Source = Source.fromResource("exerciseInputs/input_d04.txt")

@main
def run_4_1(): Unit = printWithRuntime(exercise1(input))

@main
def run_4_2(): Unit = printWithRuntime(exercise2(input))
