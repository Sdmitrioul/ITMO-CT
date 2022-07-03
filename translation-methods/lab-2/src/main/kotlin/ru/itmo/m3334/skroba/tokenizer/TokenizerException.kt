package ru.itmo.m3334.skroba.tokenizer

import java.io.IOException

class TokenizerException(override val message: String): IOException(message)
