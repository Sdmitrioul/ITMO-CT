import kotlin.math.pow

fun main() {
    val (kx, ky) = readLine()!!.split(' ').map { it.toInt() }
    val n = readLine()!!.toInt()

    val obj = mutableListOf<Pair<Int, Int>>()

    for (i in 1..n) {
        val input = readLine()!!.split(' ').map { it.toInt() - 1 }
        obj.add(Pair(input[0], input[1]))
    }

    val first = MutableList(kx) { 0.0 }
    val second = MutableList(ky) { 0.0 }
    val pairs = mutableMapOf<Pair<Int, Int>, Double>()

    val part: Double = 1.0 / n

    obj.forEach {
        first[it.first] += part
        second[it.second] += part
        pairs.merge(it, 1.0) { f, s -> f + s }
    }

    val ans = pairs.entries.fold(n.toDouble()) { acc, entry ->
        val el = n * first[entry.key.first] * second[entry.key.second]
        acc - el + (entry.value - el).pow(2) / el
    }

    println(ans)
}