import scala.::

case class Board(in: Seq[String]) {

  val rows = in.flatMap(_.split(',').map(_.split(' ').filter(_.nonEmpty).map(_.toInt).toSeq))
  val columns = rows.transpose

  def isBingo = {
    rows.exists(row => row.forall(_ == -1)) ||
      columns.exists(column => column.forall(_ == -1))
  }

  def draw(number: Int): Board = {
    val newRows = rows.map(_.map {
      case num if num == number => -1
      case num => num
    })

    Board(newRows.map(r => r.mkString(" ")))
  }
}

def prepareBoards(boards: List[Board], data: Seq[String]): List[Board] = {
  if (data.isEmpty)
    boards
  else {
    prepareBoards(Board(data.take(5)) :: boards, data.drop(5))
  }
}

def getBingoBoard(boards: List[Board], lastDrawn:Int, input: Seq[Int]): (Int, Board) = {
  if (boards.exists(_.isBingo))
    (lastDrawn, boards.find(_.isBingo).get)
  else
    getBingoBoard(boards.map(_.draw(input.head)), input.head, input.tail)
}

def run(data: Seq[String]): Int = {
  val input = data.head.split(',').filter(_.nonEmpty).map(_.toInt).toSeq

  val boards = prepareBoards(List.empty[Board], data.tail.filter(_.nonEmpty))

  val (lastDrawn, bingoBoard) = getBingoBoard(boards, 0, input)

  bingoBoard.rows.flatMap(r => r.filter(_ != -1)).sum * lastDrawn
}
