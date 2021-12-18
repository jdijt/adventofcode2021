package eu.derfniw.aoc2021.d18

import eu.derfniw.aoc2021.d18

import scala.annotation.tailrec

enum SnailNumber:
  def add(other: SnailNumber): SnailNumber = Pair(this, other).reduce
  def reduce: SnailNumber                  = SnailNumber.reduce(this)
  def magnitude: Int                       = SnailNumber.magnitude(this)

  case Pair(l: SnailNumber, r: SnailNumber)
  case Number(value: Int)
end SnailNumber

object SnailNumber:

  def fromString(in: String): SnailNumber =
    if in.forall(_.isDigit) then Number(in.toInt)
    else
      // Find the index of the comma belonging to this pair.
      val (_, pairSplitIndex) = in.zipWithIndex.foldLeft((0, -1)) {
        case ((level, foundIndex), (c, candidateIndex)) =>
          c match
            case '['                                   => (level + 1, foundIndex)
            case ']'                                   => (level - 1, foundIndex)
            case ',' if level == 1 && foundIndex == -1 => (level, candidateIndex)
            case _                                     => (level, foundIndex)
      }
      val left = in.substring(1, pairSplitIndex) // Strip first [ and drop trailing comma here..
      val right = in.substring(pairSplitIndex + 1, in.length - 1) // strip last ]
      Pair(fromString(left), fromString(right))
    end if
  end fromString

  private def magnitude(sn: SnailNumber): Int = sn match
    case Number(n)  => n
    case Pair(l, r) => (3 * magnitude(l)) + (2 * magnitude(r))

  private def addToLeftMostNumber(sn: SnailNumber, value: Int): SnailNumber = sn match
    case Number(n)  => Number(n + value)
    case Pair(l, r) => Pair(addToLeftMostNumber(l, value), r)

  private def addToRightMostNumber(sn: SnailNumber, value: Int): SnailNumber = sn match
    case Number(n)  => Number(n + value)
    case Pair(l, r) => Pair(l, addToRightMostNumber(r, value))

  @tailrec
  private def reduce(snailNumber: SnailNumber): SnailNumber =
    val (explodedNumber, eChanged, _, _) = explodeOne(snailNumber)
    if eChanged then reduce(explodedNumber)
    else
      val (split, sChanged) = splitOne(snailNumber)
      if sChanged then reduce(split)
      else snailNumber
  end reduce

  private def explodeOne(
      snailNumber: SnailNumber,
      depth: Int = 0
  ): (SnailNumber, Boolean, Option[Int], Option[Int]) =
    snailNumber match
      case Number(_) => (snailNumber, false, None, None)
      // Pair of numbers 4+ deep -> BOOM! :D
      case Pair(Number(left), Number(right)) if depth >= 4 =>
        (Number(0), true, Some(left), Some(right))
      case Pair(left, right) =>
        val (l, lChanged, lToAddLeft, lToAddRight) = explodeOne(left, depth + 1)
        val (r, rChanged, rToAddLeft, rToAddRight) =
          if !lChanged then explodeOne(right, depth + 1) else (right, false, None, None)

        // Left has changed & still a right number to add.
        // Means we update leftmost number of right branch.
        if lChanged && lToAddRight.isDefined then
          (Pair(l, addToLeftMostNumber(r, lToAddRight.get)), true, lToAddLeft, None)

        // Our right branch has changed & still a left number to add.
        // Means we update the rightmost number of the left branch.
        else if rChanged && rToAddLeft.isDefined then
          (Pair(addToRightMostNumber(l, rToAddLeft.get), r), true, None, rToAddRight)

        // Nothing we can update here, pass along.
        // This implictly covers cases like right changed & still right to add
        // Which we can't handle here, but a parent with this subtree on the left might be able to.
        else if lChanged || rChanged then (Pair(l, r), true, lToAddLeft, rToAddRight)

        // Nothing changed, return same instance.
        else (snailNumber, false, None, None)

        end if
  end explodeOne

  private def splitOne(snailNumber: SnailNumber): (SnailNumber, Boolean) = snailNumber match
    case Number(n) if n > 9 =>
      (Pair(Number((n / 2.0).floor.toInt), Number((n / 2.0).ceil.toInt)), true)
    case other: Number => (other, false)
    case Pair(left, right) =>
      val (l, lChanged) = splitOne(left)
      val (r, rChanged) = if !lChanged then splitOne(right) else (right, false)
      (Pair(l, r), lChanged || rChanged)
  end splitOne

end SnailNumber
