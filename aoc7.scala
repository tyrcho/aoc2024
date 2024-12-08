//> using jvm 21
//> using scala 3.5.2
//> using dep org.scalameta::munit:1.0.3

def main(args: Array[String]): Unit =
    println(process(io.Source.stdin))

case class Equation(target:Long, inputs:Seq[Long]) {
    @annotation.tailrec
    final def possibleValues(inputs: List[Long], acc:Set[Long]=Set.empty): Set[Long] = inputs match
        case Nil => acc
        case h::t =>
            if acc.isEmpty then possibleValues(t, Set(h))
            else possibleValues(t, acc.map(h.+) ++ acc.map(h.*))

    def score:Option[Long] =
        if possibleValues(inputs.toList)(target) then
            Some(target)
        else
            None
}

// Part 1

def process(source: io.Source): Long =
    val equations = parse(source)
    equations.flatMap(_.score).sum

def parse(source: io.Source):Seq[Equation]=
    source.getLines.map(parseLine).toSeq

def parseLine(line: String): Equation =
    val Array(left, right) = line.split(": ")
    val inputs = right.split(" ").map(_.toLong)
    Equation(left.toLong, inputs)




// Part 2

def process2(source: io.Source): Int = ???

class TestDay7Suite extends munit.FunSuite {
    val shortSample =
        """190: 10 19
          |3267: 81 40 27
          |83: 17 5
          |156: 15 6
          |7290: 6 8 6 15
          |161011: 16 10 13
          |192: 17 8 14
          |21037: 9 7 18 13
          |292: 11 6 16 20""".stripMargin

    test("part 1") {
        assertEquals(process(io.Source.fromString(shortSample)), 3749L)
    }

    test("part 2") {
        //assertEquals(process2(io.Source.fromString(shortSample)), 123)
    }

}
