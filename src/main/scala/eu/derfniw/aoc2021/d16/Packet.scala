package eu.derfniw.aoc2021.d16

import scala.annotation.tailrec
import scala.math.BigInt

sealed trait Packet:
  def version: Int
  def typeId: Int

case class LiteralValue(version: Int, value: BigInt) extends Packet:
  val typeId = 4

case class Operator(version: Int, typeId: Int, children: Seq[Packet]) extends Packet

object Packet:
  def parseLine(l: String): Packet =
    val bits = l.flatMap(_.toBits)
    parsePacket(bits)._1

  type ParseResult[A] = (A, String)
  private def parsePacket(bits: String): ParseResult[Packet] =
    val version    = bits.take(3).toIntBase2
    val typeId     = bits.slice(3, 6).toIntBase2
    val packetBits = bits.drop(6)
    if typeId == 4 then
      val (value, remainder) = parseLiteralValue(packetBits)
      (LiteralValue(version, value), remainder)
    else
      val (subPackets, remainder) = packetBits match
        case s"0$content" =>
          val length    = content.take(15).toIntBase2
          val (pkgs, _) = parseBitLine(content.slice(15, length + 15))
          (pkgs, content.drop(length + 15))
        case s"1$content" =>
          val count = content.take(11).toIntBase2
          parseNPackages(content.drop(11), count)

      (Operator(version, typeId, subPackets), remainder)
    end if
  end parsePacket

  private def parseLiteralValue(bits: String): ParseResult[BigInt] =
    val lastNumberIndex = bits.grouped(5).indexWhere(_.startsWith("0"))
    val valueSize       = 5 * (lastNumberIndex + 1)
    val value           = bits.take(valueSize).grouped(5).flatMap(_.tail).mkString.toBigIntBase2
    (value, bits.drop(valueSize))
  end parseLiteralValue

  // Parses as much packages as it can from the bitstring.
  @tailrec
  private def parseBitLine(bits: String, parsed: Seq[Packet] = Seq()): ParseResult[Seq[Packet]] =
    if bits.isEmpty then (parsed, bits)
    else
      val (packet, remainder) = parsePacket(bits)
      parseBitLine(remainder, parsed :+ packet)

  // Parses up to Count packages from bitString, returns remainder.
  @tailrec
  private def parseNPackages(
      bits: String,
      count: Int,
      parsed: Seq[Packet] = Seq()
  ): ParseResult[Seq[Packet]] =
    if count == 0 then (parsed, bits)
    else
      val (packet, remainder) = parsePacket(bits)
      parseNPackages(remainder, count - 1, parsed :+ packet)

end Packet
