
import guru.nidi.graphviz.attribute.Font
import guru.nidi.graphviz.attribute.Rank
import guru.nidi.graphviz.engine.Format
import guru.nidi.graphviz.engine.Graphviz
import guru.nidi.graphviz.model.Factory
import guru.nidi.graphviz.model.LinkSource
import kotlinFunRes.KotlinFunParser
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.nio.file.Paths

class TestKotlin {
    @Test
    fun simple() {
        val input = "fun ki()"
        val rees = KotlinFunParser(input).parse().res
        assertTrue(rees != null)

        visualize("simple", rees!!.list)
    }

    @Test
    fun notSoSimple() {
        val input = "fun ki(res: Add, tr: Gt): Unit"
        val rees = KotlinFunParser(input).parse().res
        assertTrue(rees != null)

        visualize("notSimple", rees!!.list)
    }

    companion object  {
        const val DATA_DIRECTORY = "src/main/resources"

        private fun visualize(out: String, list: List<LinkSource>) {
            val outputFile = Paths.get(DATA_DIRECTORY).resolve("output").resolve("$out.png").toFile()

            val graph = Factory.graph("result").strict().directed()
                .graphAttr().with(Rank.dir(Rank.RankDir.TOP_TO_BOTTOM))
                .nodeAttr().with(Font.name("Arial"))
                .with(
                    list.reversed()
                )

            println(list.reversed())
            Graphviz.fromGraph(graph.toMutable()).notValidating().width(1500).height(1100).render(Format.PNG).toFile(outputFile)
        }
    }
}