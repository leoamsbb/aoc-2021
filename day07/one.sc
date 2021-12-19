def calculateFuel(current: Int, ip: Seq[Int]): Int = ip
  .map(i => (current - i).abs)
  .sum

def run(data: Seq[String]): Int = {
  val positions = data
    .flatMap(_.split(",").toSeq)
    .map(_.trim.toInt)

  positions
    .map(i => calculateFuel(i, positions))
    .min
}
