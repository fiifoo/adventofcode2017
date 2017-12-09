import scala.io.Source

object Day9 {
  def input: Iterator[Char] = Source.fromResource("day9.txt").iter

  def one: Int = {
    val state = (input foldLeft State()) (read)

    state.buffer(1).head.total
  }

  def two: Int = {
    val state = (input foldLeft State()) (read)

    state.removedGarbage
  }

  private def read(state: State, char: Char): State = {
    if (state.skip) {
      state.copy(skip = false)
    } else if (state.garbage) {
      readGarbage(state, char)
    } else {
      readNormal(state, char)
    }
  }

  private def readNormal(state: State, char: Char): State = {
    char match {
      case '{' => state.copy(level = state.level + 1)
      case '}' => state.copy(level = state.level - 1, buffer = bufferGroup(state))
      case '<' => state.copy(garbage = true)
      case _ => state
    }
  }

  private def readGarbage(state: State, char: Char): State = {
    char match {
      case '!' => state.copy(skip = true)
      case '>' => state.copy(garbage = false)
      case _ => state.copy(removedGarbage = state.removedGarbage + 1)
    }
  }

  private def bufferGroup(state: State): Buffer = {
    val (buffer, subs) = state.buffer.get(state.level + 1) map (subs => {
      (state.buffer - (state.level + 1), subs)
    }) getOrElse {
      (state.buffer, List())
    }

    val groups = buffer.getOrElse(state.level, List())
    val group = Group(state.level, state.level + subs.map(_.total).sum, subs)

    buffer + (state.level -> (group :: groups))
  }

  type Level = Int
  type Buffer = Map[Level, List[Group]]

  case class Group(value: Int, total: Int, groups: List[Group])

  case class State(level: Level = 0,
                   buffer: Buffer = Map(),
                   removedGarbage: Int = 0,
                   garbage: Boolean = false,
                   skip: Boolean = false
                  )

}
