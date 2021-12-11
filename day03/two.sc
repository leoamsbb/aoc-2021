def differentBits(input: Seq[String], position:Int): Boolean = {
  input.head.charAt(position) != input(1).charAt(position)
}

def loop(input: Seq[String], gamma: Boolean, position: Int): String = {
  if (input.size <= 2 && differentBits(input, position)) {
    if (gamma)
        input.filter(s => s.charAt(position) == '1').head
    else
        input.filter(s => s.charAt(position) == '0').head
  } else {
    val transposed = input
      .map(_.toCharArray)
      .toArray
      .transpose

    val array = transposed(position)
    val ones = array.count(_ == '1')
    val zeroes = array.count(_ == '0')

    val op = (a: Int, b: Int) => if (gamma) a >= b else a < b

    val checkForBit = if (op(ones, zeroes)) '1' else '0'

    val nextInput = input.filter(arr => arr.charAt(position) == checkForBit)

    loop(nextInput, gamma, position + 1)
  }
}

def run(data: Seq[String]): Int = {

  val oxygenGeneratorRating = loop(data, true, 0)
  val oxygenGeneratorRatingInt = Integer.parseInt(String.valueOf(loop(data, true, 0)), 2)

  val co2ScrubberRating = loop(data, false, 0)
  val co2ScrubberRatingInt = Integer.parseInt(String.valueOf(loop(data, false, 0)), 2)

  println(s"oxygenGeneratorRating : $oxygenGeneratorRating & oxygenGeneratorRatingInt: $oxygenGeneratorRatingInt")
  println(s"co2ScrubberRating : $co2ScrubberRating & co2ScrubberRatingInt: $co2ScrubberRatingInt")

  oxygenGeneratorRatingInt * co2ScrubberRatingInt
}
