import kotlin.math.abs
import kotlin.math.sign

fun main() {
    val (n, m) = readInts()

    val x = mutableListOf<List<Int>>()
    val y = mutableListOf<Int>()

    for (i in 0 until n) {
        val vec = readInts()

        x.add(vec.subList(0, m))
        y.add(vec.last())
    }

    val a = readInts()

    x.forEachIndexed { index, vec ->
        val vectorValue: Long = getVectorValue(a, vec.subList(0, m))
        println(gradient(vectorValue.toDouble(), vec, y[index].toDouble()).joinToString(separator = " "))
    }
}

fun readInts(): List<Int> = readLine()!!.split(" ").map { it.toInt() }

fun getVectorValue(line: List<Int>, vector: List<Int>): Long = vector.foldIndexed(0L) { index, acc, value -> acc + (value.toLong() * line[index]) } + line.last()

fun gradient(vectorValue: Double, x: List<Int>, value: Double): List<Double> {
    val sumAbs = abs(vectorValue) + abs(value)
    val c = (sign(vectorValue - value)  - abs(vectorValue - value) * sign(vectorValue) / sumAbs) / sumAbs

    val grad = MutableList(x.size + 1) { 0.0 }

    grad[x.size] = c
    x.forEachIndexed { index, i ->
        grad[index] = i * c
    }

    return grad
}
