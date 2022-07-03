package printers

import grammar.Grammar
import grammar.RegexRuleImpl
import grammar.TerminalRuleImpl
import printers.BasePrinter.Companion.NL
import printers.BasePrinter.Companion.print

import java.io.BufferedWriter

class LexerPrinter(private val grammar: Grammar, override val grammarName: String, override val path: String) : BasePrinter {
    override val fileName: String
        get() = grammarName + "Lexer"

    override fun generateInner(printer: BufferedWriter) {
        printImport(printer)

        printer.write("class $fileName(private val input: String) {$NL")

        getSize(printer)
        getFields(printer)
        getInit(printer)

        print(printer, "fun getToken(): ${grammarName}Token? = token")
        print(printer, "fun getTokenValue(): String = match.group()")
        print(printer, "fun getPos(): Int = position")

        getSkip(printer)
        getNextToken(printer)

        printer.write("}$NL")
    }

    private fun getNextToken(printer: BufferedWriter) {
        print(printer, "fun nextToken(): ${grammarName}Token {")
        print(printer, "skip()", 2)
        print(printer, "if (position == size) {", 2)
        print(printer, "token = ${grammarName}Token.END", 3)
        print(printer, "return ${grammarName}Token.END", 3)
        print(printer, "}", 2)
        print(printer, "${grammarName}Token.values().forEach {", 2)
        print(printer, "match.usePattern(map[it])", 3)
        print(printer, "match.region(position, size)", 3)
        print(printer, "if (match.lookingAt()) {", 3)
        print(printer, "position += match.end() - match.start()", 4)
        print(printer, "token = it", 4)
        print(printer, "return it", 4)
        print(printer, "}", 3)
        print(printer, "}", 2)
        print(
            printer,
            "throw ${grammarName}LexerException(\"Illegal sequence: \${input.substring(position)}\", position)",
            2
        )
        print(printer, "}")
    }

    private fun getSkip(printer: BufferedWriter) {
        print(printer, "private fun skip() {")
        print(printer, "match.usePattern(skip)", 2)
        print(printer, "match.region(position, size)", 2)
        print(printer, "if (match.lookingAt()) {", 2)
        print(printer, "position = match.end()", 3)
        print(printer, "}", 2)
        print(printer, "}")
    }

    private fun getInit(printer: BufferedWriter) {
        print(printer, "init {")

        grammar.terminalsRules.forEach {
            if (it is RegexRuleImpl) {
                print(printer, "map[${grammarName}Token.${it.parent}] = Pattern.compile(${it.child})", 2)
                return@forEach
            }

            if (it is TerminalRuleImpl) {
                val reg: String = buildString {
                    it.child.substring(1, it.child.length - 1).toCharArray().forEach { char ->
                        if ("^$".contains(char)) {
                            append("\\\\")
                            append(char)
                        } else {
                            append("[$char]")
                        }
                    }
                }


                print(printer, "map[${grammarName}Token.${it.parent}] = Pattern.compile(\"${reg}\")", 2)
            }
        }

        print(printer, "}")
    }

    private fun getFields(printer: BufferedWriter) {
        print(printer, "private var position: Int = 0")
        print(printer, "private var token: ${grammarName}Token? = null")
        print(
            printer,
            "private val map: MutableMap<${grammarName}Token, Pattern> = EnumMap(${grammarName}Token::class.java)"
        )
        print(printer, "private val skip: Pattern = Pattern.compile(\"[ \\n\\r\\t]+\")")
        print(printer, "private val match: Matcher = Pattern.compile(\"\").matcher(input)$NL")
    }

    private fun getSize(printer: BufferedWriter) {
        print(printer, "private val size: Int")
        print(printer, "get() {", 2)
        print(printer, "return input.length", 3)
        print(printer, "}", 2)
        print(printer, NL, 0)
    }

    private fun printImport(printer: BufferedWriter) {
        printer.write("import java.util.*$NL")
        printer.write("import java.util.regex.*$NL")
        printer.write(NL)
    }
}
