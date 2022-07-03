import java.util.Locale
import kotlin.math.abs
import kotlin.math.sign

fun main() {
    val y = readLine()!!.toInt()
    val n = readLine()!!.toInt()

    val obj = mutableListOf<Pair<Int, Int>>()
    val cl = MutableList<MutableList<Int>>(y) { mutableListOf() }

    for (i in 1..n) {
        val input = readLine()!!.split(' ').map { it.toInt() }
        cl[input[1] - 1].add(input[0])
        obj.add(input[0] to input[1] - 1)
    }

    obj.sortBy { it.first }

    val oneClass: Long = cl.fold(0L) { acc, it ->
        it.sort()
        val size = it.size
        var i = 0
        var j = size - 1
        var ac = 0L
        while (i < j) {
            ac += (it[j] - it[i]).toLong() * (j - i)

            i++
            j--
        }
        acc + ac
    }

    var all = 0L
    var i = 0
    var j = n - 1
    while (i < j) {
        all += (obj[j].first - obj[i].first).toLong() * (j - i)

        i++
        j--
    }

    val differentClasses = all - oneClass

    println(2 * oneClass)
    println(2 * differentClasses)
}