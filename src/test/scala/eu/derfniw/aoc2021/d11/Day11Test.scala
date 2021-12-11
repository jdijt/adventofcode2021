package eu.derfniw.aoc2021.d11

import scala.io.Source

class Day11Test extends munit.FunSuite:
  val input: String =
    """|5483143223
       |2745854711
       |5264556173
       |6141336146
       |6357385478
       |4167524645
       |2176841721
       |6882881134
       |4846848554
       |5283751526""".stripMargin

  test("Part 1 works per example") {
    val result = exercise1(Source.fromString(input))
    assertEquals(result, 1656)
  }

  test("Part 2 works per example") {
    val result = exercise2(Source.fromString(input))
    assertEquals(result, 195)
  }
end Day11Test
