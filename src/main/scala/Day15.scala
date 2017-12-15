object Day15 {
  private val input = (591, 393)
  private val factors = (16807, 48271)
  private val criteria = (4, 8)
  private val divider = 2147483647

  def one: Long = {
    val a = Generator(input._1, factors._1, divider)
    val b = Generator(input._2, factors._2, divider)
    val runs = 40000000

    run(a, b, runs)
  }

  def two: Long = {
    val a = Generator(input._1, factors._1, divider, criteria._1)
    val b = Generator(input._2, factors._2, divider, criteria._2)
    val runs = 5000000

    run(a, b, runs)
  }

  private def run(a: Generator, b: Generator, runs: Int) = {
    val (result, _, _) = ((0 to runs) foldLeft(0, a, b)) ((carry, _) => {
      val (result, a, b) = carry

      val na = a()
      val nb = b()

      if (na.compareValue == nb.compareValue) {
        (result + 1, na, nb)
      } else {
        (result, na, nb)
      }
    })

    result
  }

  case class Generator(value: Long, factor: Long, divider: Long, criteria: Long = 1) {
    def apply(): Generator = {
      val next = copy((value * factor) % divider)

      if (next.value % criteria == 0) {
        next
      } else {
        next()
      }
    }

    def compareValue: String = {
      val result = value.toBinaryString.takeRight(16)

      if (result.length < 16) {
        result.reverse.padTo(16, '0').reverse
      } else {
        result
      }
    }
  }

}
