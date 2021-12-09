import scala.annotation.tailrec

@tailrec
def loop(acc: Int, head: Int, data: Seq[Int]): Int = {
  if (data.isEmpty)
    acc
  else {
    val result = if (head < data.head) 1 else 0
    loop(result + acc, data.head, data.tail)
  }
}

def run(data: Seq[String]): Int = {
  val d = data.map(_.toInt)
  d.foreach(println)
  loop(0, d.head, d.tail)
}


