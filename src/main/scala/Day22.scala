import scala.io.Source

object Day22 {
  private def input: Iterator[String] = Source.fromResource("day22.txt").getLines

  private val start = Location(12, 12)

  def one: Int = {
    val infected = read
    val bursts = 10000
    val initial = State(start, Up, infected)

    def transform(status: Status): Status = if (status == Infected) Clean else Infected

    val result = ((0 until bursts) foldLeft initial) (burst(transform))

    result.infections
  }

  def two: Int = {
    val infected = read
    val bursts = 10000000
    val initial = State(start, Up, infected)

    def transform(status: Status): Status = status.next

    val result = ((0 until bursts) foldLeft initial) (burst(transform))

    result.infections
  }

  private def burst(transform: Status => Status)(state: State, i: Int): State = {
    val status = state.statuses.getOrElse(state.location, Clean)
    val nextStatus = transform(status)
    val nextDirection = status.turn(state.direction)

    state.copy(
      nextDirection(state.location),
      nextDirection,
      state.statuses + (state.location -> nextStatus),
      if (nextStatus == Infected) state.infections + 1 else state.infections
    )
  }

  private def read: Set[Location] = {
    (input.zipWithIndex flatMap (line => {
      val (row, y) = line

      row.zipWithIndex filter (_._1 == '#') map (cell => {
        val (_, x) = cell

        Location(x, y)
      })
    })).toSet
  }

  object State {
    def apply(location: Location, direction: Direction, infected: Set[Location]): State = {
      State(
        location,
        direction,
        (infected map (_ -> Infected)).toMap
      )
    }
  }

  case class State(location: Location,
                   direction: Direction,
                   statuses: Map[Location, Status],
                   infections: Int = 0
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

  sealed trait Status {
    def turn(direction: Direction): Direction

    val next: Status
  }

  case object Clean extends Status {
    val next: Status = Weakened

    def turn(direction: Direction): Direction = direction match {
      case Up => Left
      case Right => Up
      case Down => Right
      case Left => Down
    }
  }

  case object Weakened extends Status {
    val next: Status = Infected

    def turn(direction: Direction): Direction = direction
  }

  case object Infected extends Status {
    val next: Status = Flagged

    def turn(direction: Direction): Direction = direction match {
      case Up => Right
      case Right => Down
      case Down => Left
      case Left => Up
    }
  }

  case object Flagged extends Status {
    val next: Status = Clean

    def turn(direction: Direction): Direction = direction match {
      case Up => Down
      case Right => Left
      case Down => Up
      case Left => Right
    }
  }

}
