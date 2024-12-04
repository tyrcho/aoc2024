//> using jvm 21
//> using scala 3.5.2


val data = io.Source.stdin.getLines
val re = """(\d+)\s+(\d+)""".r
val (left, right) = data.map {
    case re(l, r) => (l.toInt, r.toInt)
}.toList.unzip

val total = left.map(l => l * right.count(l.==)).sum
println(total)


