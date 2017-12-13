import scala.annotation.tailrec
import scala.io.Source

object Day13 {

  private val pattern = "(\\d+): (\\d+)".r

  private def input: Iterator[String] = Source.fromResource("day13.txt").getLines

  def one: Int = {
    val firewalls = (input map readLine).toMap

    ((0 to firewalls.keys.max) flatMap (layer => {
      firewalls.get(layer) map (range => {
        if (layer % ((range - 1) * 2) == 0) layer * range else 0
      })
    })).sum
  }

  def two: Int = {
    val firewalls = (input map readLine).toMap
    val distance = firewalls.keys.max

    def attempt(delay: Int = 0): Boolean = {
      val caught = (0 to distance) exists (layer => {
        firewalls.get(layer) map (range => {
          (delay + layer) % ((range - 1) * 2) == 0
        }) getOrElse {
          false
        }
      })

      !caught
    }

    @tailrec
    def seek(delay: Int = 0): Int = {
      if (attempt(delay)) {
        delay
      } else {
        seek(delay + 1)
      }
    }

    seek()
  }

  private def readLine(line: String): (Int, Int) = {
    val pattern(layer, range) = line

    (layer.toInt, range.toInt)
  }
}
