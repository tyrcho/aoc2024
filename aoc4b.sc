//> using jvm 21
//> using scala 3.5.2

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


val textGrid: Array[String] = io.Source.stdin.getLines.toArray

val count = for {
    x <- textGrid.indices
    y <- 0 until textGrid.head.length
    if validXMas.exists(matchBlock(_, textGrid, x, y))
} yield 1

println(count.sum)


