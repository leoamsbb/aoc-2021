import scala.io.Source

case class Coords(x: Int, y: Int) {
  def xeq(other: Coords) = this.x == other.x

  def yeq(other: Coords) = this.y == other.y


}

object Coords {
  implicit def apply(x: String, y: String): Coords = {
    Coords(x.trim.toInt, y.trim.toInt)
  }
}

case class Line(from: Coords, to: Coords) {
  def isXcommon = from.x == to.x

  def isYCommon = from.y == to.y

}

object Line {

  def from(from: Coords, to: Coords): Line = {
    val line = Line(from, to)
    if (line.isXcommon && from.y > to.y)
      Line(to, from)
    else if (line.isYCommon && from.x > to.x)
      Line(to, from)
    else
      line
  }

  def overlapping(l1: Line, l2: Line): Seq[Coords] = {
    val res = if (l1.from.x == l2.from.x && l1.to.x == l2.to.x && l1.from.x == l1.to.x) {
      val resultX = l1.from.x
      val sortedY = Seq(l1, l2).flatMap(l => Seq(l.from.y, l.to.y)).sorted
      val minY = sortedY(1)
      val secondLastY = sortedY(2)
      for {
        y <- minY to secondLastY
      } yield Coords(resultX, y)
    } else if (l1.from.y == l2.from.y && l1.to.y == l2.to.y && l1.from.y == l1.to.y) {
      val sortedX = Seq(l1, l2).flatMap(l => Seq(l.from.x, l.to.x)).sorted
      val minX = sortedX(1)
      val secondLastX = sortedX(2)
      val resultY = l1.from.y

//      println(
//        s"""
//           |minx: $minX
//           |secondLastX: $secondLastX
//           |resultY: $resultY
//           |""".stripMargin)
      for {
        x <- minX to secondLastX
      } yield Coords(x, resultY)
    } else Seq.empty


//    println(s"Overlapping result : $res")
    res
  }

  def intersecting(l1: Line, l2: Line): Option[Coords] = {
    val res = if (l1.from.x <= l2.from.x &&
      l1.to.x >= l2.to.x &&
      l1.from.y >= l2.from.y &&
      l1.to.y <= l2.to.y)
      Option(Coords(l2.from.x, l1.from.y))
    else if (l2.from.x <= l1.from.x &&
      l2.to.x >= l1.to.x &&
      l2.from.y >= l1.from.y &&
      l2.to.y <= l1.to.y)
      Option(Coords(l1.from.x, l2.from.y))
    else if (l1.from == l2.to)
      Option(l1.from)
    else if (l1.to == l2.from)
      Option(l1.to)
    else None

//    println(s"Intersecting result : $res")
    res
  }


  def isXCommon(line: Line, current: Coords) = {
    line.isXcommon && line.from.x == current.x
  }

  def isYCommon(line: Line, current: Coords) = {
    line.isYCommon && line.from.y == current.y
  }

  def isPointOnLine(line: Line, current: Coords) = {
    if (isXCommon(line, current)) {
      current.y >= line.from.y && current.y <= line.to.y
    } else {
      current.x >= line.from.x && current.x <= line.to.x
    }
  }
}

def run(data: Seq[String]): Int = {

  import Line._

  val input = data.map {
    case s"$x1,$y1 -> $x2,$y2" => from(Coords(x1, y1), Coords(x2, y2))
    case _ =>
      println(s"Something's wrong. Inside default case")
      Line(Coords(0, 0), Coords(0, 0))
  }.filter(l => l.isXcommon || l.isYCommon)

  /*println(s"input :")
  input.foreach(println)*/
  //  println(s"input : $input")

  def findIntersections(intersections: Set[Coords], current: Line, remaining: Seq[Line]): Set[Coords] = {
//    println(s"received intersections: $intersections")
    if (remaining.isEmpty)
      intersections
    else {
      val r = input.filterNot(current == _).flatMap { line =>
//        println(s"current: $current   & Line:$line")
        val overlappingCoords = overlapping(current, line)
        if (overlappingCoords.isEmpty) {
          intersecting(current, line).map {
            case c =>
              intersections + c
          }
        } else overlappingCoords.map(c => intersections + c)
      }

      //      println(s"r: ${r} ")
      //      println(s"r Flattened: ${r.flatten.toMap} ")
      findIntersections(r.flatten.toSet ++ intersections, remaining.head, remaining.tail)
    }
  }


  val result = findIntersections(Set.empty, input.head, input.tail)
//  println(s"result: $result")
  result.size



  //
  //  2
}
