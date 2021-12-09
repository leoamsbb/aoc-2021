
def loop(acc: Int, head:Int, data: Seq[Int]):Int = {
  if (data.isEmpty)
    acc
  else {
    val currentSum = head + data.take(2).sum
    val nextSum = data.take(3).sum
    val result = if (currentSum < nextSum) 1 else 0
    loop(result + acc, data.head, data.tail)
  }
}

def run(data: Seq[String]): Int = {
  val d = data.map(line =>
    line.split(" ").toSeq.filter(_.nonEmpty)
  ).map(_.head.toInt)

  loop(0, d.head, d.tail)
}