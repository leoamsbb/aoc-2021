
/*
    1 = 2
    7 = 3
    4 = 4
    8 = 7

    2 = 5
    3 = 5
    5 = 5

    0 = 6
    9 = 6
    6 = 6
 */

type MySet = Set[Char]

def run(data: Seq[String]): Int = {
  val input: Seq[(Seq[MySet], Seq[MySet])] = data.map(line => {
    val Array(input, output) = line.split(" \\| ")
    val inputs = input.split(" ").map(_.trim).toSeq.map(_.toSet)
    val outputs = output.split(" ").map(_.trim).toSeq.map(_.toSet)
    (inputs, outputs)
  })

  input.map { case (patterns, outputs) =>
    val one = patterns.find(_.size == 2).get
    val four = patterns.find(_.size == 4).get
    val seven = patterns.find(_.size == 3).get
    val eight = patterns.find(_.size == 7).get

    val three = patterns.find(set => set.size == 5 && one.subsetOf(set)).get
    val nine = patterns.find(set => set.size == 6 && three.subsetOf(set)).get

    val diff = eight.diff(nine).head
    val two = patterns.find(set => set.size == 5 && set.contains(diff)).get
    val five = patterns.find(set => set.size == 5 && !set.contains(diff) && set != three).get

    val six = patterns.find(set => set.size == 6 && five.subsetOf(set) && set.contains(diff)).get
    val zero = patterns.find(set => set.size == 6 && set != nine && set != six).get

    val numbers = Seq(zero, one, two, three, four, five, six, seven, eight, nine).zipWithIndex

    val r = outputs.map(s =>
      numbers
        .find(c => c._1.subsetOf(s) && c._1.size == s.size)
        .get
        ._2
    )

    r.mkString("").toInt
  }.sum
}
