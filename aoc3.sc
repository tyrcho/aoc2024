//> using jvm 21
//> using scala 3.5.2


val mulRe = """mul\((\d+),(\d+)\)""".r

val products = for {
    line <- io.Source.stdin.getLines
    List(x, y) <- mulRe.findAllIn(line).matchData.map(_.subgroups.map(_.toInt))
} yield x * y


println(products.sum)


