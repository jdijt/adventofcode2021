package eu.derfniw.aoc2021.d17

import scala.io.Source

class Day17Test extends munit.FunSuite:

  test(s"Part 1 works per example") {
    assertEquals(exercise1(Target(20, 30, -10, -5)), 45)
  }
  test(s"Part 2 works per example") {
    assertEquals(exercise2(Target(20, 30, -10, -5)), 112)
  }

end Day17Test
