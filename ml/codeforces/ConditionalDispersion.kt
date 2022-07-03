import kotlin.math.pow

fun main() {
    val x = readLine()!!.toInt()
    val n = readLine()!!.toInt()

    val obj = mutableListOf<Pair<Int, Int>>()

    for (i in 1..n) {
        val input = readLine()!!.split(' ').map { it.toInt() - 1 }
        obj.add(Pair(input[0], input[1]))
    }

    val part = 1.0 / n

    var ey = 0.0
    val ch = MutableList(x) { 0.0 }
    val ych = MutableList(x) { 0.0 }

    obj.forEach {
        ey += it.second.toDouble().pow(2) * part
        ch[it.first] += part
        ych[it.first] += it.second.toDouble() * part
    }

    val eey = ych.map { it.pow(2) }.zip(ch).filter { it.second != 0.0 }.map { it.first / it.second }.sum()

    println(ey - eey)
}