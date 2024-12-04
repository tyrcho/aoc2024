//> using jvm 21
//> using scala 3.5.2


val mulRe = """mul\((\d+),(\d+)\)""".r

def sumProduct(data: String): Int =
    mulRe.findAllIn(data).matchData
        .map(_.subgroups.map(_.toInt))
        .map(_.product)
        .sum

val doWord = "do()"
val dontWord = "don't()"

@scala.annotation.tailrec
def keepActive(data: String, active: Boolean = true, activeBlocks: List[String] = Nil): List[String] =
    if active then
        val endOfBlock = data.indexOf(dontWord)
        if endOfBlock >= 0 then
            val activeBlock = data.take(endOfBlock)
            keepActive(data.drop(endOfBlock + dontWord.length), false, activeBlock :: activeBlocks)
        else data :: activeBlocks
    else
        val endOfBlock = data.indexOf(doWord)
        if endOfBlock >= 0 then keepActive(data.drop(endOfBlock + doWord.length), true, activeBlocks)
        else activeBlocks


val activeBlocks = keepActive(io.Source.stdin.getLines.mkString)
println(activeBlocks.map(sumProduct).sum)


