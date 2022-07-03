data class DataObject(val features: List<Int>, val value: Int)

fun main() {
    val (m, k, h) = readLine()!!.split(' ').map { it.toInt() }
    val n = readLine()!!.toInt()

    val train = mutableListOf<DataObject>()

    for (i in 0 until n) {
        val input = readLine()!!.split(' ').map { it.toInt() }

        train.add(DataObject(input.subList(0, m), input[m] - 1))
    }

    val tree = Tree(train)

    println(tree)
}

class Tree(train: MutableList<DataObject>) {


    
}
