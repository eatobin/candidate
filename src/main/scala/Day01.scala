// scala-cli run . --main-class Day01
object Day01:
  private def fuel(mass: Int): Int = (mass / 3) - 2
  private def tsiolkovsky(mass: Int): Int =
    if mass < 7 then 0 else fuel(mass) + tsiolkovsky(fuel(mass))

  def part1(input: Seq[Int]): Int = input.map(fuel).sum
  def part2(input: Seq[Int]): Int = input.map(tsiolkovsky).sum

  def main(args: Array[String]): Unit =
    val data = io.Source
      .fromResource("Day01.txt")
      .getLines()
      .map(_.toInt)
      .toSeq
    println(part1(data)) // 3337766
    println(part2(data)) // 5003788
