package eu.derfniw.aoc2021.d6

import scala.io.Source

class Day6Test extends munit.FunSuite:

  val input: String =
    """3,4,3,1,2""".stripMargin

  test("Part 1 works per example") {
    val result = exercise1(Source.fromString(input))
    assertEquals(result, 5934L)
  }

  test("Part 2 works per example") {
    val result = exercise2(Source.fromString(input))
    assertEquals(result, 26984457539L)
  }
end Day6Test
