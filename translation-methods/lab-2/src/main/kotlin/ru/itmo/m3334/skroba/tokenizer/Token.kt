package ru.itmo.m3334.skroba.tokenizer

import ru.itmo.m3334.skroba.tokenizer.TokenType.*

data class Token(val type: TokenType, val value: String = "#") {
    override fun toString(): String {
        return when (type) {
            NAME, TNAME -> value
            else -> type.toString()
        }
    }
}
