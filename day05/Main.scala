// using resourceDir "./data"

import scala.io.Source

object Main extends App {
  val data_puzzle_input_one = Source
    .fromResource("data_puzzle_input_one")
    .getLines
    .toSeq

  println("Part one: " + one.run(data_puzzle_input_one))

  val data_puzzle_input_two = Source
    .fromResource("data_puzzle_input_two")
    .getLines
    .toSeq

//  println("Part two: " + two.run(data_puzzle_input_two))
}
