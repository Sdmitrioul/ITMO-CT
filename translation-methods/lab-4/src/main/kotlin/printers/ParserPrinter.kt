package printers

import grammar.*
import grammar.Grammar.Companion.EPS
import printers.BasePrinter.Companion.NL
import printers.BasePrinter.Companion.print
import java.io.BufferedWriter

class ParserPrinter(private val grammar: Grammar, override val grammarName: String, override val path: String): BasePrinter {
    override val fileName: String
        get() = grammarName + "Parser"

    private val exception = "throw ${grammarName}ParserException(\"Wrong token: \${lexer.getToken()}\", lexer.getPos())"

    override fun generateInner(printer: BufferedWriter) {
        printer.write("import ${path}.${grammarName}Token.*$NL")

        grammar.imports.forEach {
            printer.write("import $it$NL")
        }

        printer.write(NL)

        printer.write("class ${grammarName}Parser(private val input: String) {$NL")
        print(printer, "private val lexer = ${grammarName}Lexer(input)")
        printer.write(NL)

        getCheck(printer)
        getParse(printer)

        grammar.notTerminalsRules.forEach { getRules(printer, it) }

        printer.write("}$NL")
    }

    private fun getRules(printer: BufferedWriter, rule: NotTerminalRule) {
        print(printer, "class ${rule.parent.fc()} {")
        rule.getFields().forEach { print(printer, it, 2) }
        print(printer, "}")
        printer.write(NL)

        val arguments: String = rule.arguments.joinToString(separator = ", ")

        print(printer, "private fun ${rule.parent}($arguments) : ${rule.parent.fc()} {")
        print(printer, "val result =  ${rule.parent.fc()}()", 3)
        print(printer, "when (lexer.getToken()) {", 3)

        getCases(printer, rule)

        print(printer, "else -> $exception", 4)
        print(printer, "}", 3)
        print(printer, "return result", 3)
        print(printer, "}$NL")
    }

    private fun getCases(printer: BufferedWriter, rule: NotTerminalRule) {
        var rightWithEPS: List<RuleToken>? = null

        rule.tokens.forEach {
            val firsts = grammar.firstSetByRP(it)

            if (firsts.contains(EPS)) {
                rightWithEPS = it
                return@forEach
            }

            val firstsStr = firsts.joinToString(separator = ", ")

            print(printer, "$firstsStr -> {", 4)

            getCase(printer, it)
        }

        rightWithEPS?.let {
            val follows = grammar.follow[rule.parent]?.joinToString(separator = ", ") ?: ""
            print(printer, "$follows -> {", 4)
            getCase(printer, it)
        }
    }

    private fun getCase(printer: BufferedWriter, tokens: List<RuleToken>) {
        tokens.forEach {
            when (it) {
                is Code -> {
                    print(printer, it.code.trim().let { s -> s.substring(1, s.length - 1) }, 5)
                }
                is Terminal -> {
                    print(printer, "val ${it.name} = check(${grammarName}Token.${it.name})", 5)
                }
                is NotTerminal -> {
                    print(printer, "val ${it.name} = ${it.name}(${it.args.joinToString(separator = ", ")})", 5)
                }
            }
        }
        print(printer, "}", 4)
    }

    private fun getParse(printer: BufferedWriter) {
        val startRule: NotTerminalRule =
            grammar.notTerminalsRules.find { it.parent == grammar.start } ?: throw WrongGrammarException()
        val arguments: String = startRule.arguments.joinToString(separator = ", ")
        val withoutType = startRule.arguments.joinToString(separator = ", ") { it.name }

        print(printer, "fun parse(${arguments}) : ${startRule.parent.fc()} {")
        print(printer, "lexer.nextToken()", 2)
        print(printer, "val result = ${startRule.parent}(${withoutType})", 2)
        print(printer, "if (lexer.getToken() != END) {", 2)
        print(printer, exception, 3)
        print(printer, "}", 2)
        print(printer, "return result", 2)
        print(printer, "}")
    }

    private fun getCheck(printer: BufferedWriter) {
        print(printer, "private fun check(token: ${grammarName}Token) : String {")
        print(printer, "if (lexer.getToken() != token) {", 2)
        print(printer, exception, 3)
        print(printer, "}", 2)
        print(printer, "if (token == END) {", 2)
        print(printer, "return \"\"", 3)
        print(printer, "}", 2)
        print(printer, "val result = lexer.getTokenValue()", 2)
        print(printer, "lexer.nextToken()", 2)
        print(printer, "return result", 2)
        print(printer, "}")
        printer.write(NL)
    }

    private fun String.fc() = this[0].uppercaseChar() + this.substring(1)
}