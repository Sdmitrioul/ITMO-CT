fun main() {
    val (n, k) = readLine()!!.split(" ").map { it.toInt() }

    val unknown = (1..n).toList().shuffled()

    var index = 0
    var right = k
    var prefix = ""

    while (right != n) {
        if (index + 1 == n) {
            println(prefix + unknown.last())
            System.out.flush()
            break
        }

        val first = unknown[index]
        val second = unknown[index + 1]
        index += 2

        println("$prefix$first $second")
        System.out.flush()

        prefix = ""

        val nVal = readLine()!!.toInt()

        when (nVal - right) {
            2 -> {
                right += 2
            }
            0 -> {
                println(if (Math.random() < 0.5) first else second)
                System.out.flush()

                val fixed = readLine()!!.toInt()

                if (fixed < nVal) {
                    prefix = "$first $second "
                }

                right += 1
            }
            -2 -> {
                prefix = "$first $second "
            }
        }
    }

    if (prefix.isNotEmpty()) {
        println(prefix)
        System.out.flush()
    }
}

// 4 3

// 1 0 1 1
// 0 0 1 0
//

//--------------------

// 4 2

// 0 1 1 0
// 0 1 0 1
// 0 1 0 0
// 0 1 1 1
// 1 0 1 1
// 1 1 1 1

//--------------------

// 4 1

// 0 0 1 0
// 1 0 1 1
// 1 1 0 1
// 1 0 0 1

//--------------------

// 7 4

// 0 0 1 1 0 1 1
// 1 0 1 1 0 1 0
// 0 0 1 1 0 1 0
// 1 0 1 1 0 1 1
// 1 1 0 1 0 1 1
// 1 0 0 1 0 1 1
// 1 1 1 1 0 1 1
// 1 1 1 0 1 1 1
// 1 1 1 0 0 1 1
// 1 1 1 1 1 1 1




