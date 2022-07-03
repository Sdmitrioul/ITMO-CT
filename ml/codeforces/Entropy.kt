import kotlin.math.ln

fun main() {
    val (kx, _) = readLine()!!.split(' ').map { it.toInt() }
    val n = readLine()!!.toInt()

    val obj = mutableListOf<Pair<Int, Int>>()

    for (i in 1..n) {
        val input = readLine()!!.split(' ').map { it.toInt() - 1 }
        obj.add(Pair(input[0], input[1]))
    }

    val first = MutableList(kx) { 0.0 }
    val pairs = mutableMapOf<Pair<Int, Int>, Double>()

    val part: Double = 1.0 / n

    obj.forEach {
        first[it.first] += part
        pairs.merge(it, part) { f, s -> f + s }
    }

    val ans = pairs.entries.fold(0.0) { acc, entry ->
        val el = entry.value / first[entry.key.first]
        acc + ln(el) * (-entry.value)
    }

    println(ans)
}