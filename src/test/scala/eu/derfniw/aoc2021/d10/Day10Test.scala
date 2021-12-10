package eu.derfniw.aoc2021.d10

import scala.io.Source

class Day10Test extends munit.FunSuite:
  val input: String =
    """|[({(<(())[]>[[{[]{<()<>>
       |[(()[<>])]({[<{<<[]>>(
       |{([(<{}[<>[]}>{[]{[(<()>
       |(((({<>}<{<{<>}{[]{[]{}
       |[[<[([]))<([[{}[[()]]]
       |[{[{({}]{}}([{[{{{}}([]
       |{<[[]]>}<{[{[{[]{()[[[]
       |[<(<(<(<{}))><([]([]()
       |<{([([[(<>()){}]>(<<{{
       |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin

  test("Part 1 works per example") {
    val result = exercise1(Source.fromString(input))
    assertEquals(result, 26397)
  }

  test("Part 2 works per example") {
    val result = exercise2(Source.fromString(input))
    assertEquals(result, 288957L)
  }
end Day10Test
