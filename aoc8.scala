//> using jvm 21
//> using scala 3.5.2
//> using dep org.scalameta::munit:1.0.3

def main(args: Array[String]): Unit =
    println(process(io.Source.stdin))

case class Game(data: Seq[String]) {
    val ids: Set[Char] = data.flatMap(_.toSeq.toSet).toSet - '.'

    def positionsFor(id: Char): Seq[Pos] =
        for {
            row <- data.indices
            col <- data(row).indices
            if data(row)(col) == id
        } yield Pos(row, col)

    def validSymetrics: Set[Pos] =
        for {
            id <- ids
            Seq(p1, p2) <- positionsFor(id).combinations(2)
            symetric <- p1.symetrics(p2)
            if inGrid(symetric)
        } yield symetric

    def inGrid(p: Pos) = p.r >= 0 && p.c >= 0 && p.r < data.size && p.c < data.head.size

}

case class Pos(r: Int, c: Int) {
    def symetrics(that: Pos) =
        Set(
            Pos(2 * this.r - that.r, 2 * this.c - that.c),
            Pos(2 * that.r - this.r, 2 * that.c - this.c)
        )
}

def process(source: io.Source): Int =
    val game = parse(source)
    game.validSymetrics.size


def parse(source: io.Source): Game =
    Game(source.getLines.toSeq)


class TestDay8Suite extends munit.FunSuite {
    val shortSample =
        """............
          |........0...
          |.....0......
          |.......0....
          |....0.......
          |......A.....
          |............
          |............
          |........A...
          |.........A..
          |............
          |............""".stripMargin

    test("part 1") {
        assertEquals(process(io.Source.fromString(shortSample)), 14)
    }


}

