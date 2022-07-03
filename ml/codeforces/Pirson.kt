import kotlin.math.pow
import kotlin.math.sqrt

fun main() {
    val n = readLine()!!.toInt()

    val obj = mutableListOf<Pair<Int, Int>>()

    for (i in 1..n) {
        val (x, y) = readLine()!!.split(' ').map { it.toInt() }
        obj.add(x to y)
    }

    val xAvg = obj.sumOf { it.first.toLong() }.toDouble() / n
    val yAvg = obj.sumOf { it.second.toLong() }.toDouble() / n

    var nom = 0.0
    var fst = 0.0
    var scd = 0.0

    obj.forEach {
        val xDif = it.first.toLong() - xAvg
        val yDif = it.second.toLong() - yAvg

        nom += xDif * yDif
        fst += xDif.pow(2)
        scd += yDif.pow(2)
    }

    val denominator = sqrt(fst * scd)

    println(if (denominator == 0.0) 0 else nom / denominator)
}