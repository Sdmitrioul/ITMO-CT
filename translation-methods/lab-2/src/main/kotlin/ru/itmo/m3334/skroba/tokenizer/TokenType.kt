package ru.itmo.m3334.skroba.tokenizer

enum class TokenType(private val value: String) {
    START("S"),
    FUN("fun"),
    NAME("name"),
    LB("("),
    PAR("PAR"),
    OAB("<"),
    CAB(">"),
    ARGS("ARGS"),
    ARGSR("ARGSR"),
    ARGP("ARGP"),
    RB(")"),
    COLON(":"),
    COMMA(","),
    TYPE("TYPE"),
    TYPEM("TYPEM"),
    TYPER("TYPER"),
    TYPENAME("TYPEName"),
    TNAME("typeName"),
    END("$");

    override fun toString(): String {
        return value
    }
}