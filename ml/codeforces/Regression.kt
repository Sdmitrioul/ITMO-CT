import kotlin.math.PI
import kotlin.math.E
import kotlin.math.abs
import kotlin.math.pow
import kotlin.math.sqrt
import kotlin.math.cos
import kotlin.math.max

fun main() {
    val nm = readLine()?.split(" ")?.map { it.toInt() }
    val n = nm!![0]
    val m = nm[1]

    val x = mutableListOf<MutableList<Int>>()
    val y = mutableListOf<Int>()

    val find = mutableListOf<Int>()

    for (i in 0 until n) {
        val line = readLine()?.split(" ")?.map { it.toInt() }

        y.add(line!!.last())
        x.add(mutableListOf())
        line.subList(0, m).forEach { x[i].add(it) }
    }

    readLine()?.split(" ")?.map { it.toInt() }?.forEach { find.add(it) }

    val metricName = readLine() ?: ""
    val kernelName = readLine() ?: ""
    val windowName = readLine() ?: ""
    val windowIndex = readLine()?.toInt() ?: 0

    val metric = getDistance(metricName)
    val kernel = getKernel(kernelName)

    val distances = calculateDistances(metric, x, find)
    val sorted = distances.sorted()

    val window = if (windowName == "fixed") windowIndex.toDouble() else sorted[windowIndex]

    var den = 0.0
    var sum = 0.0

    for (i in distances.indices) {
        val calc = if (window == 0.0) 0.0 else distances[i] / window
        val buff = kernel.callback(calc)
        sum += y[i] * buff
        den += buff
    }

    print(if (den == 0.0) y.sumOf { it.toDouble() } / y.size else sum / den)
}

fun calculateDistances(
    distance: Distance,
    matrix: List<List<Int>>,
    vector: List<Int>
): List<Double> {
    val ans = mutableListOf<Double>()

    for (vec in matrix) {
        ans.add(distance.callback(vec, vector))
    }

    return ans
}

fun lessThanOne(value: Double, result: Double): Double = if (abs(value) < 1) result else 0.0

fun getDistance(name: String): Distance {
    return when (name) {
        "manhattan" -> Distance.Manhattan
        "euclidean" -> Distance.Euclidean
        "chebyshev" -> Distance.Chebyshev
        else -> Distance.Euclidean
    }
}

fun getKernel(name: String): Kernel {
    return Kernel.values().find { kernel -> kernel.name.lowercase() == name } ?: Kernel.Quartic
}

enum class Distance(val callback: (vector: List<Int>, ans: List<Int>) -> Double) {
    Manhattan({ vec, ans ->
        var answer = 0.0
        vec.forEachIndexed { index, value -> answer += abs(value - ans.getOrElse(index) { 0 }) }
        answer
    }),
    Euclidean({ vec, ans ->
        var answer = 0.0
        vec.forEachIndexed { index, value -> answer += (value - ans.getOrElse(index) { 0 }).toDouble().pow(2) }
        sqrt(answer)
    }),
    Chebyshev({ vec, ans ->
        var answer = 0
        vec.forEachIndexed { index, value -> answer = max(answer, abs(value - ans.getOrElse(index) { 0 })) }
        answer.toDouble()
    }),
}

enum class Kernel(val callback: (value: Double) -> Double) {
    Uniform({ value -> if (value >= 1) 0.0 else 0.5 }),
    Triangular({ value -> lessThanOne(value, 1.0 - abs(value)) }),
    Epanechniko({ value -> lessThanOne(value, 0.75 * (1 - value.pow(2.0))) }),
    Quartic({ value -> lessThanOne(value, (15.0 / 16.0) * (1 - value.pow(2.0)).pow(2.0)) }),
    Triweight({ value -> lessThanOne(value, (35.0 / 32.0) * (1 - value.pow(2.0)).pow(3.0)) }),
    Tricube({ value -> lessThanOne(value, (70.0 / 81.0) * (1 - abs(value).pow(3.0)).pow(3.0)) }),
    Gaussian({ value -> (1.0 / (2.0 * PI).pow(0.5)) * E.pow((-0.5) * value.pow(2.0)) }),
    Cosine({ value -> lessThanOne(value, (PI / 4.0) * cos(PI * value / 2.0)) }),
    Logistic({ value -> 1.0 / (E.pow(value) + 2.0 + E.pow(-value)) }),
    Sigmoid({ value -> 2.0 / (PI * (E.pow(value) + E.pow(-value))) })
}