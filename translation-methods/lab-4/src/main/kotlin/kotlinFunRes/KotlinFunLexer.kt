package kotlinFunRes

import java.util.*
import java.util.regex.*

class KotlinFunLexer(private val input: String) {
 private val size: Int
  get() {
   return input.length
  }


 private var position: Int = 0
 private var token: KotlinFunToken? = null
 private val map: MutableMap<KotlinFunToken, Pattern> = EnumMap(KotlinFunToken::class.java)
 private val skip: Pattern = Pattern.compile("[ \n\r\t]+")
 private val match: Matcher = Pattern.compile("").matcher(input)

 init {
  map[KotlinFunToken.FUN] = Pattern.compile("[f][u][n]")
  map[KotlinFunToken.OP] = Pattern.compile("[(]")
  map[KotlinFunToken.CP] = Pattern.compile("[)]")
  map[KotlinFunToken.COMMA] = Pattern.compile("[,]")
  map[KotlinFunToken.SEMICOLON] = Pattern.compile("[:]")
  map[KotlinFunToken.NAME] = Pattern.compile("[a-z]+[a-zA-Z]*")
  map[KotlinFunToken.OBJ] = Pattern.compile("[A-Z]+[a-zA-Z]*")
  map[KotlinFunToken.END] = Pattern.compile("\\$")
 }
 fun getToken(): KotlinFunToken? = token
 fun getTokenValue(): String = match.group()
 fun getPos(): Int = position
 private fun skip() {
  match.usePattern(skip)
  match.region(position, size)
  if (match.lookingAt()) {
   position = match.end()
  }
 }
 fun nextToken(): KotlinFunToken {
  skip()
  if (position == size) {
   token = KotlinFunToken.END
   return KotlinFunToken.END
  }
  KotlinFunToken.values().forEach {
   match.usePattern(map[it])
   match.region(position, size)
   if (match.lookingAt()) {
    position += match.end() - match.start()
    token = it
    return it
   }
  }
  throw KotlinFunLexerException("Illegal sequence: ${input.substring(position)}", position)
 }
}
