import scala.io.Source

object Day10 {
  private lazy val input: String = Source.fromResource("day10.txt").getLines.next

  def one: Int = {
    val lengths = input.split(',').map(_.toInt).toList
    val state = (lengths foldLeft State()) (step)
    val result = normalize(state)

    result take 2 reduce (_ * _)
  }

  def two: String = {
    val lengths = input.map(_.toInt).toList ::: List(17, 31, 73, 47, 23)
    val state = ((0 until 64) foldLeft State()) ((state, _) => {
      (lengths foldLeft state) (step)
    })
    val sparse = normalize(state)
    val dense = ((0 until 16) foldLeft(List[Int](), sparse)) ((carry, _) => {
      val (dense, sparse) = carry
      val (group, rest) = sparse splitAt 16
      val number = group reduceLeft (_ ^ _)

      (number :: dense, rest)
    })._1.reverse

    (dense map (_.toHexString.reverse.padTo(2, '0').reverse)).mkString
  }

  private def normalize(state: State): List[Int] = {
    val offset = state.offset % state.numbers.size
    val (first, last) = state.numbers splitAt (state.numbers.size - offset)

    last ::: first
  }

  private def step(state: State, length: Int): State = {
    val twisted = twist(state.numbers, length)
    val skipped = skip(twisted, state.skip)

    state.copy(
      numbers = skipped,
      skip = state.skip + 1,
      offset = state.offset + length + state.skip
    )
  }

  private def twist(numbers: List[Int], length: Int): List[Int] = {
    val (first, last) = numbers splitAt length

    last ::: first.reverse
  }

  private def skip(numbers: List[Int], amount: Int): List[Int] = {
    val (first, last) = numbers splitAt (amount % numbers.size)

    last ::: first
  }

  case class State(numbers: List[Int] = (0 to 255).toList,
                   skip: Int = 0,
                   offset: Int = 0
                  )

}
