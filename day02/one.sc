
def run(data: Seq[String]): Int = {
  val (horizontal, vertical) = data
    .map {
      case s"forward $number" => (Some(number.toInt), None)
      case s"down $number" => (None, Some(number.toInt))
      case s"up $number" => (None, Some(number.toInt  * -1))
    }
    .foldLeft((0, 0)) { (acc, current) =>
      current match {
        case (Some(number), None) => acc.copy(_1 = acc._1 + number)
        case (None, Some(number)) => acc.copy(_2 = acc._2 + number)
        case _ => acc
      }
    }

  horizontal * vertical
}
