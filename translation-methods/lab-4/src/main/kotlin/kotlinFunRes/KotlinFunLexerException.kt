package kotlinFunRes

import java.text.ParseException

class KotlinFunLexerException(override val message: String, errorOffset: Int) : ParseException(message, errorOffset)
