def summation(n: Int): Int = (n * (n + 1)) / 2

def calculateFuel(current: Int, ip: Seq[Int]): Int = ip
  .map(i => (current - i).abs)
  .map(summation)
  .sum

def run(data: Seq[String]): Int = {
  val positions = data
    .flatMap(_.split(",").toSeq)
    .map(_.trim.toInt)

  (0 to positions.max)
    .map(i => calculateFuel(i, positions))
    .min
}
