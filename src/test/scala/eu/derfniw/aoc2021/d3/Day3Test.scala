package eu.derfniw.aoc2021.d3

import scala.io.Source

class Day3Test extends munit.FunSuite {

  private val testInput =
    """|""".stripMargin

  test("Part 1 is ok per example") {
    val result = exercise1(Source.fromString(testInput))

    assertEquals(result, 7)
  }

  test("Part 2 is ok per example") {
    val result = exercise2(Source.fromString(testInput))

    assertEquals(result, 5)
  }
}
