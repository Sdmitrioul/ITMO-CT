import java.io.File

fun main() {
    val FILE_NAME = "solution.txt"

    val set = hashSetOf<String>()
    val graph = hashMapOf<String, MutableList<String>>()

    File(FILE_NAME).bufferedReader().use {
        var line: String? = it.readLine()

        while (line != null) {
            if (!line.trim().startsWith("[")) {
                line = it.readLine()
                continue
            }

            val parts = line.trim().trim('#').split(" -> ")

            parts.forEach{ part -> set.add(part.trim()) }

            var list = graph[parts[0].trim()]

            if (list == null) {
                list = mutableListOf()
                graph[parts[0].trim()] = list
            }

            list.add(parts[1].trim())

            line = it.readLine()
        }
    }

    set.sorted().forEach { pos -> println("#5 $pos") }
    println("# ${set.size}")

    val tryToFind = listOf("[P2,Q4,1,1]", "[P1,Q4,0,1]")

    val shown = hashSetOf<String>()

    set.sorted().forEach { pos ->
        dfs(tryToFind, pos, graph, shown)
    }
}

fun dfs(tryFind: List<String>, vertex: String, graph: Map<String, List<String>>, shown: HashSet<String>, visited: MutableList<String> = mutableListOf()) {
    if (visited.any{ t -> t == vertex}) return

    val newVisited: MutableList<String> = mutableListOf(vertex, *visited.toTypedArray())

    if (tryFind.any{ t -> t == vertex}){

        if (!shown.contains(newVisited.last())) {
            println("# " + newVisited.reversed().joinToString(" -> "))
            shown.add(newVisited.last())
        }
        return
    }

    graph[vertex]?.forEach {
        dfs(tryFind, it, graph, shown, newVisited)
    }
}