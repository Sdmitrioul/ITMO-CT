package arithmeticRes

import java.text.ParseException

class ArithmeticLexerException(override val message: String, errorOffset: Int) : ParseException(message, errorOffset)
