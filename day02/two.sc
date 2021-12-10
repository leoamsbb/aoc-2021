
case class Result(forward: Int = 0, depth: Int = 0, aim: Int = 0)

def run(data: Seq[String]): Int = {
  val result = data
    .map {
      case s"forward $number" => (Some(number.toInt), None)
      case s"down $number" => (None, Some(number.toInt))
      case s"up $number" => (None, Some(number.toInt * -1))
    }
    .foldLeft(Result()) { (acc, current) =>
      current match {
        case (Some(number), None) =>
          if (acc.aim > 0)
            acc.copy(forward = acc.forward + number, depth = acc.depth + acc.aim * number)
          else
            acc.copy(forward = acc.forward + number)
        case (None, Some(number)) =>
          acc.copy(aim = acc.aim + number)
        case _ => acc
      }
    }

  result.forward * result.depth
}
