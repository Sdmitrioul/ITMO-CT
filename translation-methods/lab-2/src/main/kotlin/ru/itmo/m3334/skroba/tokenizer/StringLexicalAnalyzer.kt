package ru.itmo.m3334.skroba.tokenizer

class StringLexicalAnalyzer(private val input: String): LexicalAnalyzer {
    private var position = 0
    private var token: Token? = null

    private fun back() {
        position--
    }

    private fun nextChar() = input[position++]

    private fun hasNextChar() = position < input.length

    private fun skipWhitespaces() {
        while (hasNextChar() && input[position].isWhitespace()) {
            position++
        }
    }

    private fun check(c: Char): Boolean {
        if (input[position] == c) {
            position++
            return true
        }

        return false
    }

    private fun readString(char: Char) = buildString {
        append(char)
        while (hasNextChar()) {
            val next = nextChar()

            if (next in LETTERS || next in CAPITAL_LETTERS) {
                append(next)
                continue
            }

            back()
            break
        }
    }

    override fun nextToken(): Token {
        if (position == input.length) {
            val token = Token(TokenType.END)
            this.token = Token(TokenType.END)
            return token
        }

        var char = nextChar()

        if (char.isWhitespace()) {
            skipWhitespaces()
            char = nextChar()
        }

        if (char == 'f' && check('u') && check('n')) {
            val token = Token(TokenType.FUN, "fun")
            this.token = token
            return token
        }

        val token = when(char) {
            '<' -> Token(TokenType.OAB, "<")
            '>' -> Token(TokenType.CAB, ">")
            '(' -> Token(TokenType.LB, "(")
            ')' -> Token(TokenType.RB, ")")
            ':' -> Token(TokenType.COLON, ":")
            ',' -> Token(TokenType.COMMA, ",")
            in LETTERS -> Token(TokenType.NAME, readString(char))
            in CAPITAL_LETTERS -> Token(TokenType.TNAME, readString(char))
            else -> throw TokenizerException("Unexpected symbol at pos: $position, symbol: $char")
        }

        this.token = token
        return token
    }

    override fun curToken(): Token {
        return token ?: throw TokenizerException("Null pointer exception!")
    }

    companion object {
        private val LETTERS = 'a'..'z'
        private val CAPITAL_LETTERS = 'A'..'Z'
    }
}