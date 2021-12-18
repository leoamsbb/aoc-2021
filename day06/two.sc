
def calculate(acc: Map[Int, BigInt], numOfDays: Int): BigInt = {
  println(s"acc: $acc")
  if (numOfDays == 0)
    acc.foldLeft(BigInt(0)){
      case (total, (_, big)) => total + big
    }
  else {
    val next = acc.flatMap {
      case (timer, _) if timer == 0 =>
        val numOfFish_6 = acc.getOrElse(6, BigInt(0))
        val f = (6, numOfFish_6 + 1) :: Nil
        println(s"f: $f")
        f
      case (timer, currentNumber) =>
        val numOfFish = acc.getOrElse(timer-1, BigInt(0))
        val s = (timer-1, numOfFish + 1) :: (timer, currentNumber-1) :: Nil
        println(s"s: $s")
        s
    }
    calculate(next, numOfDays - 1)
  }
}

def run(data: Seq[String], numOfDays: Int = 256): BigInt = {
  val input = data
    .flatMap(line => line.split(","))
    .map(_.trim)
    .groupMapReduce(_.toInt)(_ => BigInt(1))(_ + _)

  calculate(input, numOfDays)
}
