// using resourceDir "./data"
// using lib org.scalatest::scalatest:3.2.10

import scala.io.Source

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers._

class OneSpec extends AnyFreeSpec {
  val testData = Source
    .fromResource("data_test_one")
    .getLines
    .toSeq

  "run" - {
    "default test" in {
      val result = one.run(testData)
      result mustEqual 5
    }

    "overlapping lines scenario 1" in {
      val result = one.run(Seq(
        "1,2 -> 1,10",
        "1,4 -> 1,15"
      ))

      result mustEqual 7
    }

    "overlapping lines with starting point common" in {
      val result = one.run(Seq(
        "1,2 -> 1,15",
        "1,2 -> 1,10"
      ))

      result mustEqual 9
    }

    "overlapping lines with end point common" in {
      val result = one.run(Seq(
        "1,2 -> 1,10",
        "1,4 -> 1,10"
      ))

      result mustEqual 7
    }

    "intersecting lines at start point" in {
      val result = one.run(Seq(
        "1,2 -> 1,10",
        "1,2 -> 4,2"
      ))

      result mustEqual 1
    }

    "intersecting lines at end point" in {
      val result = one.run(Seq(
        "1,2 -> 1,5",
        "0,5 -> 5,5"
      ))

      result mustEqual 1
    }
  }
}
