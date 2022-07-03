package util

fun fac(n : Int): Int {
    var res = 1

    for (i in 1..n) res *= i

    return res
}