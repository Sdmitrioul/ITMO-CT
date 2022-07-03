package ru.itmo.m3334.skroba.tokenizer

interface LexicalAnalyzer {
    fun nextToken(): Token
    fun curToken(): Token
}