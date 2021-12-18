
def calculate(acc: Seq[Int], numOfDays: Int): Seq[Int] = {
  if (numOfDays == 0)
    acc
  else {
    val next = acc.flatMap {
      case 0 => 6 :: 8 :: Nil
      case x => (x - 1) :: Nil
    }
    calculate(next, numOfDays - 1)
  }
}

def run(data: Seq[String], numOfDays: Int = 80): Int = {
  val input = data
    .flatMap(line => line.split(","))
    .map(_.trim)
    .map(_.toInt)

  calculate(input, numOfDays).size
}
