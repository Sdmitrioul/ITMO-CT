import kotlin.math.pow

fun main() {
    val n = readLine()!!.toInt()
    val count = 2.0.pow(n).toInt()

    val funRes = mutableListOf<Int>()

    for (i in 1..count) {
        funRes.add(readLine()!!.toInt())
    }

    val posCount = funRes.count { it == 1 }

    if (posCount > 512) {
        buildCnf(funRes, n)
        return
    }

    buildDnf(funRes, n)
}

fun buildCnf(funRes: MutableList<Int>, n: Int) {
    val list = mutableListOf<MutableList<Double>>()

    funRes.forEachIndexed { index, i ->
        if (i == 0) list.add(createOr(n, index))
    }

    println(2)
    println("${list.size} 1")
    list.forEach { println(it.joinToString(separator = " ")) }
    printDefVector(list.size, 1 - list.size)
}

fun buildDnf(funRes: MutableList<Int>, n: Int) {
    val list = mutableListOf<MutableList<Double>>()

    funRes.forEachIndexed { index, i ->
        if (i == 1) list.add(createAnd(n, index))
    }

    if (list.isEmpty()) {
        println('1')
        println('1')
        printDefVector(n)
        return
    }

    println(2)
    println("${list.size} 1")
    list.forEach { println(it.joinToString(separator = " ")) }
    printDefVector(list.size)
}

fun createOr(n: Int, i: Int): MutableList<Double> = createNeuron(n, i, -1)

fun createAnd(n: Int, i: Int): MutableList<Double> = createNeuron(n, i, 1)

fun createNeuron(n: Int, i: Int, sign: Int): MutableList<Double> {
    val res = mutableListOf<Double>()

    (0 until n).forEach {
        if (((i shr it) and 1) == 1) {
            res.add(sign * 1.0)
        } else {
            res.add(sign * (-1.0))
        }
    }

    res.add(sign * (0.5 - i.countOneBits()))

    return res
}

fun printDefVector(n: Int, add: Int = 0) = println((1..n).map { 1.0 }.joinToString(separator = " ") + " ${0 - 0.5}")
