import scala.io.Source

object Day20 {
  private def input: Iterator[String] = Source.fromResource("day20.txt").getLines

  def one: Int = {
    val particles = read

    particles
      .zipWithIndex
      .minBy(_._1.acceleration.sum)
      ._2
  }

  def two: Int = {
    val particles = read
    val ticks = 10000

    val remaining = ((0 until ticks) foldLeft particles) (tick)

    remaining.size
  }

  private def tick(particles: List[Particle], i: Int): List[Particle] = {
    val moved = particles map (_.tick)

    val (_, collisions) = (moved foldLeft(Set[Values](), Set[Values]())) ((carry, particle) => {
      val (positions, collisions) = carry

      if (positions contains particle.position) {
        (positions, collisions + particle.position)
      } else {
        (positions + particle.position, collisions)
      }
    })

    if (collisions.isEmpty) {
      moved
    } else {
      moved filterNot (collisions contains _.position)
    }
  }

  private def read: List[Particle] = {
    input.toList map Particle.read
  }

  case class Values(x: Int, y: Int, z: Int) {
    def add(other: Values): Values = {
      Values(x + other.x, y + other.y, z + other.z)
    }

    def sum: Int = math.abs(x) + math.abs(y) + math.abs(z)
  }

  case class Particle(position: Values, velocity: Values, acceleration: Values) {
    def tick: Particle = {
      val nextVelocity = velocity add acceleration
      val nextPosition = position add nextVelocity

      copy(nextPosition, nextVelocity)
    }
  }

  object Values {
    private val pattern = "<(-?\\d+),(-?\\d+),(-?\\d+)>".r

    def read(source: String): Values = {
      val pattern(x, y, z) = source

      Values(x.toInt, y.toInt, z.toInt)
    }
  }

  object Particle {
    private val pattern = "p=(.+), v=(.+), a=(.+)".r

    def read(source: String): Particle = {
      val pattern(position, velocity, acceleration) = source

      Particle(Values.read(position), Values.read(velocity), Values.read(acceleration))
    }
  }

}
