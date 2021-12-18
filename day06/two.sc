
def timerTick(pop: Map[Int, BigInt]): Map[Int, BigInt] = {
  def currentCount(timer:Int): BigInt = pop.getOrElse(timer, BigInt(0))
  Map(
    0 -> currentCount(1),
    1 -> currentCount(2),
    2 -> currentCount(3),
    3 -> currentCount(4),
    4 -> currentCount(5),
    5 -> currentCount(6),
    6 -> (currentCount(7) + currentCount(0)),
    7 -> currentCount(8),
    8 -> currentCount(0)
  )
}

def run(data: Seq[String], numOfDays: Int = 256): BigInt = {
  val input = data
    .flatMap(line => line.split(","))
    .groupMapReduce(_.trim.toInt)(_ => BigInt(1))(_ + _)

  (1 to numOfDays)
    .foldLeft(input)((pop, _) => timerTick(pop))
    .values
    .sum
}
