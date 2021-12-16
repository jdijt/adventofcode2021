package eu.derfniw.aoc2021.d16

import scala.io.Source

class Day16Test extends munit.FunSuite:
  val part1Cases: Seq[(String, Int)] = Seq(
    "8A004A801A8002F478"             -> 16,
    "620080001611562C8802118E34"     -> 12,
    "C0015000016115A2E0802F182340"   -> 23,
    "A0016C880162017C3686B18A3D4780" -> 31
  )

  part1Cases.foreach { case (in, out) =>
    test(s"Part 1 works per example $in") {
      assertEquals(exercise1(Source.fromString(in)), out)
    }
  }

  val part2Cases: Seq[(String, BigInt)] = Seq(
    "C200B40A82"                 -> BigInt(3),
    "04005AC33890"               -> BigInt(54),
    "880086C3E88112"             -> BigInt(7),
    "CE00C43D881120"             -> BigInt(9),
    "D8005AC2A8F0"               -> BigInt(1),
    "F600BC2D8F"                 -> BigInt(0),
    "9C005AC2F8F0"               -> BigInt(0),
    "9C0141080250320F1802104A08" -> BigInt(1)
  )
  part2Cases.foreach { case (in, out) =>
    test(s"Part 2 works per example $in") {
      assertEquals(exercise2(Source.fromString(in)), out)
    }
  }

end Day16Test
