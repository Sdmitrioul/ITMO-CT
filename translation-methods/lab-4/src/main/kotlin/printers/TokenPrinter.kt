package printers

import grammar.Grammar
import printers.BasePrinter.Companion.NL
import printers.BasePrinter.Companion.print


import java.io.BufferedWriter

class TokenPrinter(private val grammar: Grammar, override val grammarName: String, override val path: String): BasePrinter {
    override val fileName: String
        get() = grammarName + "Token"

    override fun generateInner(printer: BufferedWriter) {
        printer.write("enum class $fileName {$NL")
        grammar.terminalsRules.forEach {
            print(printer, "${it.parent},", 1)
        }
        printer.write("}$NL")
    }
}