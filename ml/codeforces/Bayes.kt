import kotlin.math.exp
import kotlin.math.ln

fun main() {
    val classCounts = readLine()!!.toInt()
    val penalties = readLine()!!.split(' ').map { it.toInt() }
    val alpha = readLine()!!.toInt()

    val trainCount = readLine()!!.toInt()
    val xTrain = mutableListOf<Set<String>>()
    val yTrain = mutableListOf<Int>()

    val wordsInClass = (1..classCounts).map { HashMap<String, Int>() }.toList()
    val countOfObjects = (1..classCounts).map { 0 }.toMutableList()
    val words = HashSet<String>()
    val probability = mutableListOf<Double>()

    for (i in 1..trainCount) {
        val input = readLine()!!.split(' ')
        val y = input[0].toInt() - 1
        val l = input[1].toInt()
        yTrain.add(y)
        xTrain.add(input.subList(2, 2 + l).toHashSet())

        countOfObjects[y]++
        words.addAll(xTrain.last())

        xTrain.last().forEach {
            wordsInClass[y].merge(it, 1) { prev, value -> prev + value }
        }
    }

    val testCount = readLine()!!.toInt()
    val xTest = mutableListOf<Set<String>>()

    for (i in 1..testCount) {
        val input = readLine()!!.split(' ')
        val l = input[0].toInt()
        xTest.add(input.subList(1, l + 1).toHashSet())
    }

    (0 until classCounts).forEach {
        probability.add(ln((penalties[it] * countOfObjects[it]).toDouble() / xTrain.size))
    }

    val wordProbability: (word: String, c: Int) -> Double = { word, cl ->
        (wordsInClass[cl].getOrDefault(word, 0) + alpha).toDouble() / (countOfObjects[cl] + alpha * 2)
    }

    val predict: (i: Set<String>) -> List<Double> = { testWords ->
        val res = MutableList(classCounts) { 0.0 }

        (0 until classCounts).forEach {
            if (countOfObjects[it] == 0) return@forEach

            val currentProbability = words.sumOf { word ->
                val wordProb: Double = wordProbability(word, it)

                if (testWords.contains(word)) {
                    return@sumOf if (wordProb !=  0.0) ln(wordProb) else 0.0
                }

                return@sumOf if (wordProb != 1.0) ln(1 - wordProb) else 0.0
            } + probability[it]

            res[it] = currentProbability
        }

        val result = mutableListOf<Double>()

        for (i in 0 until classCounts) {
            var p = 0.0

            if (countOfObjects[i] != 0) {
                var denom = 1.0
                for (j in 0 until classCounts) {
                    if (!(i == j ||countOfObjects[j] == 0)) {
                        denom += exp(res[j] - res[i])
                    }
                }
                p = 1.0 / denom
            }

            result.add(p)
        }

        result
    }

    xTest.forEach {
        println(predict(it).joinToString(separator = " "))
    }
}
