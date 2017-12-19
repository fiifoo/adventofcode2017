import scala.annotation.tailrec
import scala.io.Source

object Day19 {
  private def input: Iterator[String] = Source.fromResource("day19.txt").getLines

  def one: String = {
    val map = read
    val initial = State(
      location = Location(1, 0),
      direction = Down,
      steps = 1
    )

    travel(map, initial).letters
  }

  def two: Int = {
    val map = read
    val initial = State(
      location = Location(1, 0),
      direction = Down,
      steps = 1
    )

    travel(map, initial).steps
  }

  @tailrec
  private def travel(map: Map[Location, Char], state: State): State = {
    val to = state direction state.location

    val next = map get to collect {
      case '|' | '-' => state.copy(
        location = to,
        steps = state.steps + 1
      )
      case '+' => state.copy(
        location = to,
        direction = turn(map, to, state.direction),
        steps = state.steps + 1
      )
      case letter => state.copy(
        location = to,
        letters = state.letters + letter,
        steps = state.steps + 1
      )
    } getOrElse {
      state.copy(end = true)
    }

    if (next.end) {
      next
    } else {
      travel(map, next)
    }
  }

  private def turn(map: Map[Location, Char], location: Location, current: Direction): Direction = {
    current match {
      case Up | Down if map get Right(location) exists turnHorizontal => Right
      case Up | Down if map get Left(location) exists turnHorizontal => Left
      case Right | Left if map get Up(location) exists turnVertical => Up
      case Right | Left if map get Down(location) exists turnVertical => Down
    }
  }

  private def turnVertical(char: Char): Boolean = {
    char match {
      case '|' | '+' => true
      case _ => false
    }
  }

  private def turnHorizontal(char: Char): Boolean = {
    char match {
      case '-' | '+' => true
      case _ => false
    }
  }

  private def read: Map[Location, Char] = {
    (input.zipWithIndex flatMap (row => {
      val (chars, y) = row

      chars.zipWithIndex flatMap (cell => {
        val (char, x) = cell

        if (char == ' ') {
          None
        } else {
          Some(Location(x, y) -> char)
        }
      })
    })).toMap
  }

  case class State(location: Location,
                   direction: Direction,
                   steps: Int,
                   letters: String = "",
                   end: Boolean = false
                  )

  case class Location(x: Int, y: Int)

  sealed trait Direction {
    def apply(location: Location): Location
  }

  object Up extends Direction {
    def apply(location: Location): Location = Location(location.x, location.y - 1)
  }

  object Right extends Direction {
    def apply(location: Location): Location = Location(location.x + 1, location.y)
  }

  object Down extends Direction {
    def apply(location: Location): Location = Location(location.x, location.y + 1)
  }

  object Left extends Direction {
    def apply(location: Location): Location = Location(location.x - 1, location.y)
  }

}
