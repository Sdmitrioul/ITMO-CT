package arithmeticRes

import java.text.ParseException

class ArithmeticParserException(override val message: String, errorOffset: Int) : ParseException(message, errorOffset)
