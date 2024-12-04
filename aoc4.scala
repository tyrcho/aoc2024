//> using jvm 21
//> using scala 3.5.2
//> using dep org.scalameta::munit:1.0.3

// Part 1

@scala.annotation.tailrec
def searchWord(word: List[Char], grid: Array[String], x: Int, y: Int, dx: Int, dy: Int): Boolean =
    word match {
        case Nil => true
        case h :: t =>
            if x >= 0 && y >= 0 && x < grid.length && y < grid(x).length && grid(x)(y) == h then
                searchWord(t, grid, x + dx, y + dy, dx, dy)
            else false
    }

val word = "XMAS".toList

def process4(source: io.Source) =
    val textGrid: Array[String] = source.getLines.toArray
    (for {
        x <- textGrid.indices
        y <- 0 until textGrid.head.length
        dx <- -1 to 1
        dy <- -1 to 1
        if (dx, dy) != (0, 0)
        if searchWord(word, textGrid, x, y, dx, dy)
    } yield 1).sum

// Part 2

val validXMas = Seq(
    Seq("M.S",
        ".A.",
        "M.S"),
    Seq("M.M",
        ".A.",
        "S.S"),
    Seq("S.M",
        ".A.",
        "S.M"),
    Seq("S.S",
        ".A.",
        "M.M"))

def matchBlock(block: Seq[String], grid: Array[String], x: Int, y: Int): Boolean =
    x + 2 < grid.length && y + 2 < grid.head.length && x >= 0 && y >= 0 && (0 to 2).forall(dx =>
        (0 to 2).forall(dy =>
            block(dx)(dy) == '.' || block(dx)(dy) == grid(x + dx)(y + dy)
        )
    )


def process4b(source: io.Source) =
    val textGrid: Array[String] = source.getLines.toArray

    (for {
        x <- textGrid.indices
        y <- 0 until textGrid.head.length
        if validXMas.exists(matchBlock(_, textGrid, x, y))
    } yield 1).sum


def main(args: Array[String]): Unit =
    println(process4(io.Source.stdin))


class TestSuite extends munit.FunSuite {
    val shortSample =
        """MMMSXXMASM
          |MSAMXMSMSA
          |AMXSXMAAMM
          |MSAMASMSMX
          |XMASAMXAMM
          |XXAMMXXAMA
          |SMSMSASXSS
          |SAXAMASAAA
          |MAMMMXMMMM
          |MXMXAXMASX""".stripMargin

    test("day 4 part 1") {
        assertEquals(process4(io.Source.fromString(shortSample)), 18)
    }
    test("day 4 part 2") {
        assertEquals(process4b(io.Source.fromString(shortSample)), 9)
    }
}

