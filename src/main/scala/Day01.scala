// scala-cli run . --main-class Day01
import scala.annotation.tailrec

object Day01:
  private def fuel(mass: Int): Int = (mass / 3) - 2
  private def fuelPlusFuel(mass: Int): Int =
    @tailrec
    def fuelAccumulator(m: Int, accum: Int): Int =
      val newGas: Int = fuel(m)
      if newGas > 0
      then fuelAccumulator(m = newGas, accum = accum + newGas)
      else accum
    fuelAccumulator(m = mass, accum = 0)

  def part1(input: Seq[Int]): Int = input.map(fuel).sum
  def part2(input: Seq[Int]): Int = input.map(fuelPlusFuel).sum

  def main(args: Array[String]): Unit =
    val data = io.Source
      .fromResource("Day01.txt")
      .getLines()
      .map(_.toInt)
      .toSeq
    println(part1(data)) // 3337766
    println(part2(data)) // 5003788
