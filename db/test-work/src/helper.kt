fun main() {
    val list = mutableListOf<String>()
    while (true) {
        val input: String = readln().trim();

        if (input == "e") {
            break
        }

        list.add(input)
    }

    println(list.joinToString(separator = ",\n", transform = { "($it)" }))
}