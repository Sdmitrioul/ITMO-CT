package ru.itmo.m3334.skroba.parser

import guru.nidi.graphviz.model.Factory.*
import guru.nidi.graphviz.model.LinkSource
import ru.itmo.m3334.skroba.tokenizer.Token
import ru.itmo.m3334.skroba.tokenizer.TokenType
import ru.itmo.m3334.skroba.tokenizer.TokenType.*
import java.util.*
import kotlin.collections.ArrayList

class Tree {
    private val token: Token
    val children = LinkedList<Tree>()

    constructor(token: Token) {
        this.token = token
    }

    constructor(type: TokenType) {
        token = Token(type)
    }

    fun addChild(node: Tree) {
        children.add(node)
    }

    fun addChild(node: Token) {
        children.add(Tree(node))
    }

    fun addChild(node: TokenType) {
        children.add(Tree(node))
    }

    fun getTree(counter: Incrementor = Incrementor(), parent: String = ""): List<LinkSource> {
        counter.inc()
        val result = ArrayList<LinkSource>()
        val value = counter.value
        val him = "№ $value\n$token"

        children.forEach{
            result.addAll(it.getTree(counter, him).reversed())
        }

        if (parent.isNotEmpty()) {
            result.add(node(parent).link(node(him)))
        }

        return result.reversed()
    }

    override fun toString(): String {
        val tokenValue = when (token.type) {
            FUN, NAME, LB, RB, COLON, COMMA, TNAME, OAB, CAB -> token.toString()
            else -> ""
        }
        return tokenValue + children.joinToString("", transform = Tree::toString)
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as Tree

        if (token != other.token) return false
        var res = true
        other.children.forEachIndexed { index, tree -> res = res && this.children[index].equals(tree) }
        return res
    }

    override fun hashCode(): Int {
        var result = token.hashCode()
        children.forEach { result = 31 * result + it.hashCode() }
        return result
    }

    companion object {
        class Incrementor {
            var value = -1
            fun inc() {
                value++
            }
        }
    }
}