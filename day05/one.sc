
case class Point(x: Int, y: Int) {
  def xStep(step: Int) = copy(x = x + step)

  def yStep(step: Int) = copy(y = y + step)

}

object Point {
  implicit def apply(x: String, y: String): Point = {
    Point(x.trim.toInt, y.trim.toInt)
  }
}

case class Line(from: Point, to: Point) {
  def isXcommon = from.x == to.x

  def isYCommon = from.y == to.y

  def xStep = if (from.x < to.x) 1 else -1
  def yStep = if (from.y < to.y) 1 else -1

  def getPoints: Seq[Point] = {
    if (isXcommon) {
      val x = from.x
      for {
        y <- from.y to to.y
      } yield Point(x, y)
    } else if (isYCommon) {
      val y = from.y
      for {
        x <- from.x to to.x
      } yield Point(x, y)
    } else {
      diagonal(from, from :: Nil)
    }
  }

  def diagonal(current: Point, acc: List[Point]): List[Point] = {
    if (current == to)
      current :: acc
    else
      diagonal(current.xStep(xStep).yStep(yStep), current :: acc)
  }
}

object Line {

  def from(from: Point, to: Point): Line = {
    val line = Line(from, to)
    if (line.isXcommon && from.y > to.y)
      Line(to, from)
    else if (line.isYCommon && from.x > to.x)
      Line(to, from)
    else
      line
  }
}

def run(data: Seq[String]): Int = {

  import Line._

  val input = data.map {
    case s"$x1,$y1 -> $x2,$y2" => from(Point(x1, y1), Point(x2, y2))
    case _ =>
      println(s"Something's wrong. Inside default case")
      Line(Point(0, 0), Point(0, 0))
  }.filter(l => l.isXcommon || l.isYCommon)

  input
    .flatMap(line => line.getPoints)
    .groupBy(identity)
    .map { case (k, v) => (k, v.size) }
    .flatMap {
      case (_, v) if (v >= 2) => Option(v)
      case _ => None
    }
    .size
}
