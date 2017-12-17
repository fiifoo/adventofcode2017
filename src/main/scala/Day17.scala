object Day17 {
  private val steps: Int = 335

  def one: Int = {
    val inserts = 2017

    val (buffer, position) = ((1 to inserts) foldLeft(Vector(0), 0)) ((carry, i) => {
      val (buffer, position) = carry
      val next = ((position + steps) % i) + 1
      val (first, last) = buffer splitAt next

      (
        (first :+ i) ++ last,
        next
      )
    })

    buffer(position + 1)
  }

  def two: Int = {
    val inserts = 50000000

    val (result, _) = ((1 to inserts) foldLeft(0, 0)) ((carry, i) => {
      val (result, position) = carry
      val next = ((position + steps) % i) + 1

      if (next == 1) {
        (i, next)
      } else {
        (result, next)
      }
    })

    result
  }
}
