import kotlin.math.pow
import kotlin.math.tanh

class Matrix(private val n: Int, private val k: Int, private val a: Double) {
    private val value: MutableList<MutableList<Double>> = MutableList(n) { MutableList(k) { a } }
    val size: Pair<Int, Int>
        get() = n to k

    val T: Matrix
        get() {
            val res = Matrix(k, n)

            for (i in 0 until n)  {
                for (j in 0 until k) {
                    res[j][i] = this[i][j]
                }
            }

            return res
        }

    constructor(n: Int, k: Int): this(n, k, 0.0)


    operator fun times(matrix: Matrix): Matrix {
        val l = matrix[0].size

        val res = Matrix(n, l)

        for (i in 0 until n) {
            for (j in 0 until l) {
                for (t in 0 until k) {
                    res[i][j] += this[i][t] * matrix[t][j]
                }
            }
        }

        return res
    }

    operator fun plus(matrix: Matrix): Matrix {
        val res = Matrix(n, k)

        for (i in 0 until n) {
            for (j in 0 until k) {
                res[i][j] = matrix[i][j] + this[i][j]
            }
        }

        return res
    }

    operator fun get(i: Int): MutableList<Double> = value[i]

    fun adamar(matrix: Matrix): Matrix {
        val res = Matrix(n, k)

        for (i in 0 until n) {
            for (j in 0 until k) {
                res[i][j] = matrix[i][j] * this[i][j]
            }
        }

        return res
    }

    fun tanh(): Matrix {
        val res = Matrix(n, k)

        for (i in 0 until n) {
            for (j in 0 until k) {
                res[i][j] = tanh(this[i][j])
            }
        }

        return res
    }

    fun relu(alpha: Int): Matrix {
        val res = Matrix(n, k)

        for (i in 0 until n) {
            for (j in 0 until k) {
                val delimiter = if (this[i][j] < 0) alpha else 1
                res[i][j] = this[i][j] / delimiter
            }
        }

        return res
    }

    override fun toString(): String {
        return value.joinToString(separator = "\n") { it.joinToString(separator = " ") }
    }

    companion object {
        val ZERO = Matrix(0, 0)
    }
}

abstract class Node {
    var derivative: Matrix = Matrix.ZERO
    var value: Matrix = Matrix.ZERO

    var computedValue: Boolean = false
    var computedDerivative: Boolean = false

    fun forward(nodes: List<Node>) {
        if (computedValue) return

        calc(nodes)
        computedValue = true

        val size = value.size
        derivative = Matrix(size.first, size.second)
    }

    fun backward(nodes: List<Node>) {
        if (computedDerivative) return

        calcDerivative(nodes)
        computedDerivative = true
    }

    protected abstract fun calc(nodes: List<Node>)

    protected abstract fun calcDerivative(nodes: List<Node>)
}

class Var(private val n: Int, private val k: Int): Node() {
    init {
        value = Matrix(n, k)
    }

    override fun calc(nodes: List<Node>) {
        for (i in 0 until n) {
            val input = readLine()!!.split(' ').map { it.toDouble() }

            for (j in 0 until k) {
                value[i][j] = input[j]
            }
        }
    }

    override fun calcDerivative(nodes: List<Node>) {
        //Do nothing
    }
}

class Tanh(private val index: Int): Node() {
    override fun calc(nodes: List<Node>) {
        if (!nodes[index].computedValue) {
            nodes[index].forward(nodes)
        }

        value = nodes[index].value.tanh()
    }

    override fun calcDerivative(nodes: List<Node>) {
        val source = nodes[index].derivative
        val  (n, k) = source.size

        for (i in 0 until n) {
            for (j in 0 until k) {
                source[i][j] = source[i][j] + (1 - value[i][j].pow(2)) * derivative[i][j]
            }
        }
    }
}

class RLU(private val alpha: Int, private val index: Int): Node() {
    override fun calc(nodes: List<Node>) {
        if (!nodes[index].computedValue) {
            nodes[index].forward(nodes)
        }

        value = nodes[index].value.relu(alpha)
    }

    override fun calcDerivative(nodes: List<Node>) {
        val source = nodes[index].derivative
        val  (n, k) = source.size

        for (i in 0 until n) {
            for (j in 0 until k) {
                val delimiter = if (nodes[index].value[i][j] < 0) alpha else 1

                source[i][j] = source[i][j] + derivative[i][j] / delimiter
            }
        }
    }
}

class Mul(private val i: Int, private val j: Int): Node() {
    override fun calc(nodes: List<Node>) {
        if (!nodes[i].computedValue) {
            nodes[i].forward(nodes)
        }

        if (!nodes[j].computedValue) {
            nodes[j].forward(nodes)
        }

        value = nodes[i].value * nodes[j].value
    }

    override fun calcDerivative(nodes: List<Node>) {
        val productI = derivative * nodes[j].value.T
        val productJ = nodes[i].value.T * derivative

        nodes[i].derivative = nodes[i].derivative + productI
        nodes[j].derivative = nodes[j].derivative + productJ
    }
}

class Sum(private val list: List<Int>): Node() {
    override fun calc(nodes: List<Node>) {
        list.forEachIndexed { index, i ->
            if (!nodes[i].computedValue) {
                nodes[i].forward(nodes)
            }

            val add = nodes[i].value

            if (index == 0) {
                value = add
                return@forEachIndexed
            }

            value += add
        }
    }

    override fun calcDerivative(nodes: List<Node>) {
        list.forEach {
            nodes[it].derivative = nodes[it].derivative + derivative
        }
    }
}

class Had(private val list: List<Int>): Node() {
    override fun calc(nodes: List<Node>) {
        list.forEachIndexed { index, i ->
            if (!nodes[i].computedValue) {
                nodes[i].forward(nodes)
            }

            val mul = nodes[i].value

            if (index == 0) {
                value = mul
                return@forEachIndexed
            }

            value = value.adamar(mul)
        }
    }

    override fun calcDerivative(nodes: List<Node>) {
        list.forEachIndexed { index, v ->
            var buff = derivative

            list.forEachIndexed { indexY, vy ->
                if (index != indexY) {
                    buff = buff.adamar(nodes[vy].value)
                }
            }

            nodes[v].derivative = nodes[v].derivative + buff
        }
    }
}

class Graph() {
    val nodes: MutableList<Node> = mutableListOf()

    fun createNode(name: String, params: List<Int>) {
        when (name) {
            "var" -> nodes.add(Var(params[0], params[1]))
            "tnh" -> nodes.add(Tanh(params[0] - 1))
            "rlu" -> nodes.add(RLU(params[0], params[1] - 1))
            "mul" -> nodes.add(Mul(params[0] - 1, params[1] - 1))
            "sum" -> nodes.add(Sum(params.subList(1, params.size).map { it - 1 }))
            "had" -> nodes.add(Had(params.subList(1, params.size).map { it - 1 }))
        }
    }
}

fun main() {
    val graph = Graph()

    val (n, m, k) = readLine()!!.split(' ').map { it.toInt() }

    for (i in 1..n) {
        val input = readLine()!!.split(' ')
        graph.createNode(input[0], input.subList(1, input.size).map { it.toInt() })
    }

    graph.nodes.forEach { it.forward(graph.nodes) }

    for (i in (n - k) until n) {
        readMatrix(i, graph)
    }

    graph.nodes.asReversed().forEach {
        it.backward(graph.nodes)
    }

    for (i in (n - k) until n) {
        println(graph.nodes[i].value)
    }

    for (i in 0 until m) {
        println(graph.nodes[i].derivative)
    }
}

fun readMatrix(i: Int, graph: Graph) {
    val matrix = graph.nodes[i].derivative
    val size = matrix.size

    for (t in 0 until size.first) {
        val input = readLine()!!.split(' ').map { it.toDouble() }
        for (r in 0 until size.second) {
            matrix[t][r] = input[r]
        }
    }
}