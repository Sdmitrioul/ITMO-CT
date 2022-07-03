import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import java.io.File
import java.nio.file.Paths

val DATA_PATH = "src/main/resources"

fun main(args: Array<String>) {
    val stream = CharStreams.fromPath(Paths.get(DATA_PATH).resolve(args[0]))
    val lexer = GrammarLexer(stream)
    val tokenStream = CommonTokenStream(lexer)
    val parser = GrammarParser(tokenStream)
    val tree = parser.file()
    val translator = Translator()
    translator.visit(tree)

    File(Paths.get(DATA_PATH).resolve("out").resolve(args[0]).toString()).bufferedWriter().use {
        it.write(translator.result)
    }
    //println(translator.result)
}