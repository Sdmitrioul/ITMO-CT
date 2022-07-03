package kotlinFunRes

import java.text.ParseException

class KotlinFunParserException(override val message: String, errorOffset: Int) : ParseException(message, errorOffset)
