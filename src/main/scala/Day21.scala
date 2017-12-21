import scala.io.Source

object Day21 {
  private def input: Iterator[String] = Source.fromResource("day21.txt").getLines

  private val initial = Grid.read(".#./..#/###")

  def one: Int = {
    val rules = read

    val image = ((0 until 5) foldLeft initial) ((image, _) => {
      process(rules)(image)
    })

    (image map (_.sum)).sum
  }

  def two: Int = {
    val rules = read

    val image = ((0 until 18) foldLeft initial) ((image, _) => {
      process(rules)(image)
    })

    (image map (_.sum)).sum
  }

  private def process(rules: List[Rule])(image: Grid): Grid = {
    val size = if (image.size % 2 == 0) 2 else 3
    val parts = split(image, size)
    val converted = parts map (_ map convert(rules))

    join(converted, size + 1)
  }

  private def split(grid: Grid, size: Int): List[List[Grid]] = {
    chunks(grid, size) map (chunk => {
      val parts = chunk map (chunks(_, size))

      (0 until grid.size / size).toList map (i => {
        (0 until size).toList map (j => {
          parts(j)(i)
        })
      })
    })
  }

  private def join(grids: List[List[Grid]], size: Int): Grid = {
    grids.reverse flatMap (row => {
      (0 until size).toList map (i => {
        row.reverse flatMap (grid => {
          grid(i)
        })
      })
    })
  }

  private def chunks[T](list: List[T], size: Int): List[List[T]] = {
    val (_, result) = ((0 until list.size / size) foldLeft(list, List[List[T]]())) ((carry, _) => {
      val (list, result) = carry
      val (chunk, rest) = list splitAt size

      (rest, chunk :: result)
    })

    result
  }

  private def convert(rules: List[Rule])(grid: Grid): Grid = {
    (rules find (_ matches grid)).get.output
  }

  private def read: List[Rule] = {
    (input map Rule.read).toList
  }

  type Grid = List[List[Int]]

  object Grid {
    def read(source: String): Grid = {
      (source split '/').toList map (line => (line map (char => if (char == '#') 1 else 0)).toList)
    }
  }

  case class Rule(hashes: Set[Int], output: Grid) {
    def matches(grid: Grid): Boolean = {
      hashes contains grid.hashCode
    }
  }

  object Rule {
    private val pattern = "(.+) => (.+)".r

    def read(source: String): Rule = {
      val pattern(input, output) = source

      Rule(
        hashes(Grid.read(input)),
        Grid.read(output)
      )
    }

    private def hashes(grid: Grid): Set[Int] = {
      val (_, result) = ((0 until 4) foldLeft(grid, Set[Int]())) ((carry, _) => {
        val (grid, result) = carry

        val transposed = grid.transpose
        val reversed = transposed.reverse

        (reversed, result + transposed.hashCode + reversed.hashCode)
      })

      result
    }
  }
}
