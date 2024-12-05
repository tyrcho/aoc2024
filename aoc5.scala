//> using jvm 21
//> using scala 3.5.2
//> using dep org.scalameta::munit:1.0.3

def main(args: Array[String]): Unit =
    println(process2(io.Source.stdin))

// Part 1

def parseConstraints(l: String): (Int, Int) =
    l.split("\\|").toSeq match {
        case Seq(a, b) => (a.toInt, b.toInt)
    }

def parseOrder(l: String): Seq[Int] =
    l.split(",").toSeq.map(_.toInt)

def process(source: io.Source): Int =
    val (constraints, printOrder) = parse(source)
    val matchingPages = printOrder.map(checkOrder(constraints, _))
    matchingPages.flatten.sum

def parse(source: io.Source): (List[(Int, Int)], List[Seq[Int]]) =
    val lines = source.getLines.toList
    val sectionEnd = lines.indexWhere(_.isBlank)
    val constraints = lines.take(sectionEnd).map(parseConstraints)
    val printOrder = lines.drop(sectionEnd + 1).map(parseOrder)
    (constraints, printOrder)


def checkOrder(constraints: List[(Int, Int)], seq: Seq[Int]): Option[Int] =
    constraints match
        case Nil => Some(seq(seq.length / 2))
        case (before, after) :: tail =>
            val first = seq.indexOf(before)
            val last = seq.indexOf(after)
            if first == -1 || last == -1 || first < last then
                checkOrder(tail, seq)
            else None

// Part 2

def process2(source: io.Source): Int =
    val (constraints, printOrder) = parse(source)
    val fixed = for {
        order <- printOrder
        if checkOrder(constraints, order) == None
    } yield fixOrder(constraints, order)
    fixed.sum

def fixOrder(constraints: List[(Int, Int)], seq: Seq[Int]): Int =
    val sorted = seq.sortWith((a, b) => constraints.contains((a, b)))
    sorted(sorted.length / 2)


class TestDay5Suite extends munit.FunSuite {
    val shortSample =
        """47|53
          |97|13
          |97|61
          |97|47
          |75|29
          |61|13
          |75|53
          |29|13
          |97|29
          |53|29
          |61|53
          |97|53
          |61|29
          |47|13
          |75|47
          |97|75
          |47|61
          |75|61
          |47|29
          |75|13
          |53|13
          |
          |75,47,61,53,29
          |97,61,53,29,13
          |75,29,13
          |75,97,47,61,53
          |61,13,29
          |97,13,75,29,47""".stripMargin

    test("part 1") {
        assertEquals(process(io.Source.fromString(shortSample)), 143)
    }

    test("part 2") {
        assertEquals(process2(io.Source.fromString(shortSample)), 123)
    }

}

