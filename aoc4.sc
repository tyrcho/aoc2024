//> using jvm 21
//> using scala 3.5.2

@scala.annotation.tailrec
def searchWord(word: List[Char], grid: Array[String], x: Int, y: Int, dx: Int, dy: Int): Boolean =
    word match {
        case Nil => true
        case h :: t =>
            if x >= 0 && y >= 0 && x < grid.length && y < grid(x).length && grid(x)(y) == h then
                searchWord(t, grid, x + dx, y + dy, dx, dy)
            else false
    }

val textGrid: Array[String] = io.Source.stdin.getLines.toArray
val word = "XMAS".toList
val count = for {
    x <- textGrid.indices
    y <- 0 until textGrid.head.length
    dx <- -1 to 1
    dy <- -1 to 1
    if (dx, dy) != (0, 0)
    if searchWord(word, textGrid, x, y, dx, dy)
} yield 1

println(count.sum)


