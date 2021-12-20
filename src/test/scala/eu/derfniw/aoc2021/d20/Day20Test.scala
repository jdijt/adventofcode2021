package eu.derfniw.aoc2021.d20

import scala.io.Source

class Day20Test extends munit.FunSuite:

  test("Part 1 works per example") {
    assertEquals(exercise1(Source.fromResource("testin_d20.txt")), 35)
  }

end Day20Test
