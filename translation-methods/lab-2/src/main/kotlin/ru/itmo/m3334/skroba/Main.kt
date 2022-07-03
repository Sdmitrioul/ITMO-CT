package ru.itmo.m3334.skroba

import guru.nidi.graphviz.attribute.*
import guru.nidi.graphviz.engine.Format
import guru.nidi.graphviz.engine.Graphviz
import guru.nidi.graphviz.model.Factory.*
import ru.itmo.m3334.skroba.parser.KotlinFunParser
import ru.itmo.m3334.skroba.parser.Tree
import java.nio.file.Paths

val DATA_DIRECTORY = "src/main/resources"

fun main(args: Array<String>) {
    require(args.isNotEmpty() && args.first().isNotEmpty()){ "Arguments should contain valid file name." }

    val inputFileName = args.first()
    val input = Paths.get(DATA_DIRECTORY).resolve(inputFileName).toFile().readText()

    val result = KotlinFunParser(input).parse()

    showGraph(result)

    if (args.size == 2) {
        visualize(result, args[1])
    }
}

fun visualize(result: Tree, outputFileName: String) {
    val outputFile = Paths.get(DATA_DIRECTORY).resolve("output").resolve("$outputFileName.png").toFile()

    val graph = graph("result").strict().directed()
        .graphAttr().with(Rank.dir(Rank.RankDir.TOP_TO_BOTTOM))
        .nodeAttr().with(Font.name("Arial"))
        .with(
            result.getTree()
        )

    Graphviz.fromGraph(graph.toMutable()).notValidating().width(1400).height(1000).render(Format.PNG).toFile(outputFile);
}

fun showGraph(result: Tree) {
    println(result)
}
