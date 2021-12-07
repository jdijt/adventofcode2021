package eu.derfniw.aoc2021.d7

import scala.io.Source

class Day7Test extends munit.FunSuite:
  val input: String =
    """16,1,2,0,4,2,7,1,2,14""".stripMargin

  test("Part 1 works per example") {
    val result = exercise1(Source.fromString(input))
    assertEquals(result, 37)
  }

  test("Part 2 works per example") {
    val result = exercise2(Source.fromString(input))
    assertEquals(result, 168)
  }
end Day7Test
