package arithmeticRes

import java.util.*
import java.util.regex.*

class ArithmeticLexer(private val input: String) {
 private val size: Int
  get() {
   return input.length
  }


 private var position: Int = 0
 private var token: ArithmeticToken? = null
 private val map: MutableMap<ArithmeticToken, Pattern> = EnumMap(ArithmeticToken::class.java)
 private val skip: Pattern = Pattern.compile("[ \n\r\t]+")
 private val match: Matcher = Pattern.compile("").matcher(input)

 init {
  map[ArithmeticToken.ADD] = Pattern.compile("[+]")
  map[ArithmeticToken.SUB] = Pattern.compile("[-]")
  map[ArithmeticToken.MUL] = Pattern.compile("[*]")
  map[ArithmeticToken.LOG] = Pattern.compile("[l][o][g]")
  map[ArithmeticToken.DIV] = Pattern.compile("[/]")
  map[ArithmeticToken.OP] = Pattern.compile("[(]")
  map[ArithmeticToken.CP] = Pattern.compile("[)]")
  map[ArithmeticToken.NUM] = Pattern.compile("([1-9][0-9]*)|(0)")
  map[ArithmeticToken.FAC] = Pattern.compile("[!]")
  map[ArithmeticToken.END] = Pattern.compile("\\$")
 }
 fun getToken(): ArithmeticToken? = token
 fun getTokenValue(): String = match.group()
 fun getPos(): Int = position
 private fun skip() {
  match.usePattern(skip)
  match.region(position, size)
  if (match.lookingAt()) {
   position = match.end()
  }
 }
 fun nextToken(): ArithmeticToken {
  skip()
  if (position == size) {
   token = ArithmeticToken.END
   return ArithmeticToken.END
  }
  ArithmeticToken.values().forEach {
   match.usePattern(map[it])
   match.region(position, size)
   if (match.lookingAt()) {
    position += match.end() - match.start()
    token = it
    return it
   }
  }
  throw ArithmeticLexerException("Illegal sequence: ${input.substring(position)}", position)
 }
}
