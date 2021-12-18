package eu.derfniw.aoc2021.d18

import eu.derfniw.aoc2021.d18.SnailNumber.*

class SnailNumberTest extends munit.FunSuite:

  private val parseCases = Seq(
    "[1,2]"         -> Pair(Number(1), Number(2)),
    "[[1,2],3]"     -> Pair(Pair(Number(1), Number(2)), Number(3)),
    "[9,[8,7]]"     -> Pair(Number(9), Pair(Number(8), Number(7))),
    "[[1,9],[8,5]]" -> Pair(Pair(Number(1), Number(9)), Pair(Number(8), Number(5))),
    "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]" ->
      Pair(
        Pair(
          Pair(
            Pair(Number(1), Number(2)),
            Pair(Number(3), Number(4))
          ),
          Pair(
            Pair(Number(5), Number(6)),
            Pair(Number(7), Number(8))
          )
        ),
        Number(9)
      )
  )

  parseCases.foreach { case (in, out) =>
    test(s"Parsing case $in") {
      assertEquals(SnailNumber.fromString(in), out)
    }
  }

  test(s"Simple reduce case 1 (Split only)") {
    val in       = Pair(Number(11), Number(9))
    val expected = Pair(Pair(Number(5), Number(6)), Number(9))
    assertEquals(in.reduce, expected)
  }
  test(s"Simple reduce case 2 (Split only)") {
    val in       = Pair(Number(12), Number(9))
    val expected = Pair(Pair(Number(6), Number(6)), Number(9))
    assertEquals(in.reduce, expected)
  }
  test(s"Simple reduce case 3 (Split only)") {
    val in       = Pair(Number(23), Number(9))
    val expected = Pair(Pair(Pair(Number(5), Number(6)), Pair(Number(6), Number(6))), Number(9))
    assertEquals(in.reduce, expected)
  }
  test(s"Simple reduce case 4 (Explode only)") {
    val in =
      Pair(Pair(Pair(Pair(Pair(Number(9), Number(8)), Number(1)), Number(2)), Number(3)), Number(4))
    val expected = Pair(Pair(Pair(Pair(Number(0), Number(9)), Number(2)), Number(3)), Number(4))
    assertEquals(in.reduce, expected)
  }
  test(s"Simple reduce case 5 (Explode only)") {
    val in =
      Pair(Number(7), Pair(Number(6), Pair(Number(5), Pair(Number(4), Pair(Number(3), Number(2))))))
    val expected = Pair(Number(7), Pair(Number(6), Pair(Number(5), Pair(Number(7), Number(0)))))
    assertEquals(in.reduce, expected)
  }
  test(s"Simple reduce case 6 (Explode only)") {
    val in =
      Pair(Pair(Number(6), Pair(Number(5), Pair(Number(4), Pair(Number(3), Number(2))))), Number(1))
    val expected = Pair(Pair(Number(6), Pair(Number(5), Pair(Number(7), Number(0)))), Number(3))
    assertEquals(in.reduce, expected)
  }
  test(s"Combined reduce case") {
    val in = Pair(
      Pair(
        Pair(Pair(Pair(Number(4), Number(3)), Number(4)), Number(4)),
        Pair(Number(7), Pair(Pair(Number(8), Number(4)), Number(9)))
      ),
      Pair(Number(1), Number(1))
    )
    val out = Pair(
      Pair(
        Pair(Pair(Number(0), Number(7)), Number(4)),
        Pair(Pair(Number(7), Number(8)), Pair(Number(6), Number(0)))
      ),
      Pair(Number(8), Number(1))
    )
    assertEquals(in.reduce, out)
  }

end SnailNumberTest
