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
    "" in {
      val result = one.run(testData)
      result mustEqual 150
    }
  }
}
