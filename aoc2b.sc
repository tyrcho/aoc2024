//> using jvm 21
//> using scala 3.5.2


@scala.annotation.tailrec
def safeRec(list: List[Int], last: Int, increase: Boolean): Boolean = list match
    case h :: t =>
        if increase then h > last && h - last <= 3 && safeRec(t, h, increase)
        else h < last && last - h <= 3 && safeRec(t, h, increase)
    case Nil => true

def safe(list: List[Int]): Boolean = list match {
    case i :: j :: t =>
        val delta = (i - j).abs
        delta > 0 && delta <= 3 && safeRec(t, j, j > i)
}

def removeOne(list: List[Int]): Seq[List[Int]] =
    for (i <- 0 to list.length) yield
        list.take(i) ::: list.drop(i + 1)

def almostSafe(list: List[Int]): Int =
    if removeOne(list).exists(safe) then 1 else 0

val data = io.Source.stdin.getLines.map(_.split("\\s+").map(_.toInt).toList)

val total = data.map(almostSafe).sum
println(total)


