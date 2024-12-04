//> using jvm 21
//> using scala 3.5.2
//> using dep org.scalameta::munit:1.0.3

@scala.annotation.tailrec
def safeRec(list: List[Int], last: Int, increase: Boolean): Boolean = list match
    case h :: t =>
        if increase then h > last && h - last <= 3 && safeRec(t, h, increase)
        else h < last && last - h <= 3 && safeRec(t, h, increase)
    case Nil => true

def safe(list: List[Int]): Boolean =
    val i :: j :: t = list : @unchecked
    val delta = (i - j).abs
    delta > 0 && delta <= 3 && safeRec(t, j, j > i)



def removeOne(list: List[Int]): Seq[List[Int]] =
    for (i <- 0 to list.length) yield
        list.take(i) ::: list.drop(i + 1)

def almostSafe(list: List[Int]): Boolean =
    removeOne(list).exists(safe)


def process2(source: io.Source): Int =
    source.getLines
        .map(_.split("\\s+").map(_.toInt).toList)
        .count(safe)

def process2b(source: io.Source): Int =
    source.getLines
        .map(_.split("\\s+").map(_.toInt).toList)
        .count(almostSafe)

def main(args: Array[String]): Unit =
    println(process2b(io.Source.stdin))


class TestSuite extends munit.FunSuite {
    val shortSample =
        """7 6 4 2 1
          |1 2 7 8 9
          |9 7 6 2 1
          |1 3 2 4 5
          |8 6 4 4 1
          |1 3 6 7 9
          |""".stripMargin

    test("day 2 part 1") {
        assertEquals(process2(io.Source.fromString(shortSample)), 2)
    }

    test("day 2 part 2") {
        assertEquals(process2b(io.Source.fromString(shortSample)), 4)
    }
}

