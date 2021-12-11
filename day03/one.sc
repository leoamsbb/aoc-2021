def run(data: Seq[String]): Int = {
  val arrayOfArrays = data
    .map(_.toCharArray)
    .toArray

  val transposed = arrayOfArrays.transpose

  val r = transposed.map(array => {
    val ones = array.count(_ == '1')
    val zeroes = array.count(_ == '0')

    val gammaBit = if (ones > zeroes) '1' else '0'
    val epsilonBit = if (zeroes > ones) '1' else '0'

    (gammaBit, epsilonBit)
  })

  val gamma = Integer.parseInt(String.valueOf(r.map(_._1)), 2)
  val epsilon = Integer.parseInt(String.valueOf(r.map(_._2)), 2)

  gamma * epsilon
}
