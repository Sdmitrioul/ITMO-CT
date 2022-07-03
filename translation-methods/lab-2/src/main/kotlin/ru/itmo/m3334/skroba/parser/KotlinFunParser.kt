package ru.itmo.m3334.skroba.parser

import ru.itmo.m3334.skroba.tokenizer.StringLexicalAnalyzer
import ru.itmo.m3334.skroba.tokenizer.Token
import ru.itmo.m3334.skroba.tokenizer.TokenType
import ru.itmo.m3334.skroba.tokenizer.TokenType.*

class KotlinFunParser(input: String): Parser {
    private val tokenizer = StringLexicalAnalyzer(input)

    override fun parse(): Tree {
        tokenizer.nextToken()
        return S()
    }

    private fun S(): Tree {
        val node = Tree(START)

        node.addChild(assertToken(FUN))
        node.addChild(assertToken(NAME))
        node.addChild(assertToken(LB))
        node.addChild(ARGS())
        node.addChild(assertToken(RB))
        node.addChild(TYPEM())

        return node
    }

    private fun assertToken(type: TokenType): Tree {
        val token = tokenizer.curToken()

        if (token.type == type) {
            tokenizer.nextToken()
            return Tree(token)
        }

        throw ParserException("Waited token $type, got: ${token.type}")
    }

    private fun ARGS(): Tree {
        val result = Tree(Token(ARGS))
        val token = tokenizer.curToken()

        when (token.type) {
            NAME -> {
                result.addChild(ARGSR())
            }
            RB -> {
            }
            else -> throw ParserException("Unexpected token: ${token.type} in token ARGS")
        }

        return result
    }

    private fun ARGSR(): Tree {
        val result = Tree(ARGSR)
        val token = tokenizer.curToken()

        when (token.type) {
            NAME -> {
                result.addChild(token)
                tokenizer.nextToken()
                result.children.add(TYPE())
                result.children.add(ARGP())
            }
            RB -> {
            }
            else -> throw ParserException("Unexpected token: ${token.type} in token ARGSR")
        }

        return result
    }

    private fun TYPE(): Tree {
        val result = Tree(TYPE)
        val token = tokenizer.curToken()

        when (token.type) {
            COLON -> {
                result.addChild(token)
                tokenizer.nextToken()

                if (tokenizer.curToken().type != TNAME) {
                    throw ParserException("Unexpected token: ${token.type} in token TYPENAME")
                }

                result.addChild(TYPENAME())
            }
            else -> throw ParserException("Unexpected token: ${token.type} in token COLON")
        }

        return result
    }

    private fun TYPENAME(): Tree {
        val result = Tree(TYPENAME)
        val name = tokenizer.curToken()

        result.addChild(name)

        val oab = tokenizer.nextToken()

        if (oab.type == OAB) {
            result.addChild(oab)
            tokenizer.nextToken()

            result.addChild(TYPENAME())

            val cab = tokenizer.curToken()
            if (cab.type != CAB)  throw ParserException("Unexpected token: ${cab.type} in token CAB")
            result.addChild(cab)
            tokenizer.nextToken()
        }

        return result
    }

    private fun ARGP(): Tree {
        val result = Tree(ARGP)
        val token = tokenizer.curToken()

        when (token.type) {
            RB -> {
                return result
            }
            COMMA -> {
                result.addChild(token)
                tokenizer.nextToken()

               if (tokenizer.curToken().type == NAME) {
                    result.addChild(tokenizer.curToken())
                    tokenizer.nextToken()
                } else {
                    throw ParserException("Unexpected token: ${token.type} in token NAME")
                }

                result.addChild(TYPE())

                result.addChild(ARGP())
            }
            else -> throw ParserException("Unexpected token: ${token.type} in token ARGP")
        }

        return result
    }


    private fun TYPEM(): Tree {
        val result = Tree(TYPEM)
        val token = tokenizer.curToken()

        when (token.type) {
            END -> {
            }
            COLON -> {
                result.addChild(TYPER())
            }
            else -> throw ParserException("Unexpected token: ${token.type} in token TUPEM")
        }

        return result
    }

    private fun TYPER(): Tree {
        val result = Tree(TYPER)
        val token = tokenizer.curToken()

        when (token.type) {
            END -> {
            }
            COLON -> {
                result.addChild(TYPE())
            }
            else -> throw ParserException("Unexpected token: ${token.type} in token TUPER")
        }

        return result
    }
}