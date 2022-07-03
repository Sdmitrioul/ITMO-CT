import kotlin.math.pow

fun main() {
    val n = readLine()!!.toInt()

    val obj = mutableListOf<Pair<Int, Int>>()

    for (i in 1..n) {
        val (x, y) = readLine()!!.split(' ').map { it.toInt() }
        obj.add(x to y)
    }

    val mapX = obj.sortedBy { p -> p.first }.mapIndexed { i, pair -> pair.first to i }.toMap()
    val mapY = obj.sortedBy { p -> p.second }.mapIndexed { i, pair -> pair.second to i }.toMap()

    val buff = obj.sumOf { (mapX[it.first]!! - mapY[it.second]!!).toDouble().pow(2) }

    println(1 - 6.0 / n / (n - 1) / (n + 1) * buff)
}