package printers

import GrammarLexer
import GrammarParser
import grammar.Grammar
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import java.nio.file.Paths

class LL1Generator(private val dataPath: String, private val dataOut: String, private val name: String) {
    private val parser = GrammarParser(CommonTokenStream(GrammarLexer(CharStreams.fromPath(Paths.get(dataPath).resolve(name)))))
    val grammar: Grammar = parser.start().gr

    init {
        grammar.buildAll()
    }

    fun generateAll() {
        grammar.buildAll()

        if (grammar.checkLL1()) {
            ExceptionPrinter(name, "Lexer", dataOut).generate()
            ExceptionPrinter(name, "Parser", dataOut).generate()
            TokenPrinter(grammar, name, dataOut).generate()
            LexerPrinter(grammar, name, dataOut).generate()
            ParserPrinter(grammar, name, dataOut).generate()
        } else {
            println("Not LL1")
        }
    }
}