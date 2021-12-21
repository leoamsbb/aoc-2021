
def run(data: Seq[String]): Int = {
  val input = data.map(s => s.splitAt(s.indexOf('|')))
    .map(s => s._2)
    .flatMap(_.split(' ').toSeq)
    .map(_.trim)
    .filter(_.nonEmpty)

  input.map(_.length)
    .count(len => len == 2 || len == 4 || len == 3 || len == 7)
}
