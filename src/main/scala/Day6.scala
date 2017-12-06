import scala.annotation.tailrec
import scala.io.Source

object Day6 {
  private lazy val input: String = Source.fromResource("day6.txt").getLines.next

  def one: Int = {
    val banks = input.split("\\t") map (_.toInt)

    def calculateHash: Int = banks.toList.hashCode

    @tailrec
    def cycle(hashes: Set[Int], cycles: Int = 1): Int = {
      val blocks = banks.max
      val index = banks.indexOf(banks.max)

      var iterator = banks.indices.iterator.drop(index + 1)
      banks(index) = 0

      for (_ <- 0 until blocks) {
        val index = if (iterator.hasNext) {
          iterator.next()
        } else {
          iterator = banks.indices.iterator

          iterator.next()
        }

        banks(index) = banks(index) + 1
      }

      val hash = calculateHash

      if (hashes.contains(hash)) {
        cycles
      } else {
        cycle(hashes + hash, cycles + 1)
      }
    }

    cycle(Set(calculateHash))
  }

  def two: Int = {
    val banks = input.split("\\t") map (_.toInt)

    def calculateHash: Int = banks.toList.hashCode

    def cycle(hashes: Set[Int], wanted: Option[Int] = None, cycles: Int = 0): Int = {
      val blocks = banks.max
      val index = banks.indexOf(banks.max)

      var iterator = banks.indices.iterator.drop(index + 1)
      banks(index) = 0

      for (_ <- 0 until blocks) {
        val index = if (iterator.hasNext) {
          iterator.next()
        } else {
          iterator = banks.indices.iterator

          iterator.next()
        }

        banks(index) = banks(index) + 1
      }

      val hash = calculateHash

      if (wanted contains hash) {
        cycles
      } else if (wanted.isEmpty && hashes.contains(hash)) {
        cycle(hashes, Some(hash), 1)
      } else {
        cycle(hashes + hash, wanted, if (wanted.isDefined) cycles + 1 else 0)
      }
    }

    cycle(Set(calculateHash))
  }
}
