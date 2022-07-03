package arithmeticRes

import arithmeticRes.ArithmeticToken.*
import  util.fac 

class ArithmeticParser(private val input: String) {
 private val lexer = ArithmeticLexer(input)

 private fun check(token: ArithmeticToken) : String {
  if (lexer.getToken() != token) {
   throw ArithmeticParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
  }
  if (token == END) {
   return ""
  }
  val result = lexer.getTokenValue()
  lexer.nextToken()
  return result
 }

 fun parse() : Run {
  lexer.nextToken()
  val result = run()
  if (lexer.getToken() != END) {
   throw ArithmeticParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
  }
  return result
 }
 class Run {
  var v: Int? = null
 }

 private fun run() : Run {
   val result =  Run()
   when (lexer.getToken()) {
    SUB, OP, NUM -> {
    val addSub = addSub()
    result.v = addSub.v
    val END = check(ArithmeticToken.END)
    }
    else -> throw ArithmeticParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
   }
   return result
 }

 class AddSub {
  var v: Int? = null
 }

 private fun addSub() : AddSub {
   val result =  AddSub()
   when (lexer.getToken()) {
    SUB, OP, NUM -> {
    val mulDiv = mulDiv()
    val addSub_ = addSub_(mulDiv.v)
    result.v = addSub_.v
    }
    else -> throw ArithmeticParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
   }
   return result
 }

 class AddSub_ {
  var v: Int? = null
 }

 private fun addSub_(left: Int?) : AddSub_ {
   val result =  AddSub_()
   when (lexer.getToken()) {
    ADD -> {
    val ADD = check(ArithmeticToken.ADD)
    val mulDiv = mulDiv()
    val next = left!! + mulDiv.v!!
    val addSub_ = addSub_(next)
    result.v = addSub_.v
    }
    SUB -> {
    val SUB = check(ArithmeticToken.SUB)
    val mulDiv = mulDiv()
    val next = left!! - mulDiv.v!!
    val addSub_ = addSub_(next)
    result.v = addSub_.v
    }
    END, CP -> {
    result.v = left
    }
    else -> throw ArithmeticParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
   }
   return result
 }

 class MulDiv {
  var v: Int? = null
 }

 private fun mulDiv() : MulDiv {
   val result =  MulDiv()
   when (lexer.getToken()) {
    SUB, OP, NUM -> {
    val log = log()
    val mulDiv_ = mulDiv_(log.v)
    result.v = mulDiv_.v
    }
    else -> throw ArithmeticParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
   }
   return result
 }

 class MulDiv_ {
  var v: Int? = null
 }

 private fun mulDiv_(left: Int?) : MulDiv_ {
   val result =  MulDiv_()
   when (lexer.getToken()) {
    MUL -> {
    val MUL = check(ArithmeticToken.MUL)
    val log = log()
    val mulDiv_ = mulDiv_(left!! * log.v!!)
    result.v = mulDiv_.v
    }
    DIV -> {
    val DIV = check(ArithmeticToken.DIV)
    val log = log()
    val mulDiv_ = mulDiv_(left!! / log.v!!)
    result.v = mulDiv_.v
    }
    ADD, SUB, END, CP -> {
    result.v = left
    }
    else -> throw ArithmeticParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
   }
   return result
 }

 class Log {
  var v: Int? = null
 }

 private fun log() : Log {
   val result =  Log()
   when (lexer.getToken()) {
    SUB, OP, NUM -> {
    val unary = unary()
    val log_ = log_(unary.v)
    result.v = log_.v
    }
    else -> throw ArithmeticParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
   }
   return result
 }

 class Log_ {
  var v: Int? = null
 }

 private fun log_(left: Int?) : Log_ {
   val result =  Log_()
   when (lexer.getToken()) {
    LOG -> {
    val LOG = check(ArithmeticToken.LOG)
    val log = log()
    result.v = (Math.log(left!!.toDouble()) / Math.log(log.v!!.toDouble())).toInt()
    }
    DIV, ADD, SUB, MUL, END, CP -> {
    result.v = left
    }
    else -> throw ArithmeticParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
   }
   return result
 }

 class Unary {
  var v: Int? = null
 }

 private fun unary() : Unary {
   val result =  Unary()
   when (lexer.getToken()) {
    SUB -> {
    val SUB = check(ArithmeticToken.SUB)
    val unary = unary()
    result.v = -unary.v!!
    }
    OP, NUM -> {
    val first_pr = first_pr()
    result.v = first_pr.v
    }
    else -> throw ArithmeticParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
   }
   return result
 }

 class First_pr {
  var v: Int? = null
 }

 private fun first_pr() : First_pr {
   val result =  First_pr()
   when (lexer.getToken()) {
    OP, NUM -> {
    val first = first()
    val f = first!!.v
    val fact = fact(f)
     result.v = fact!!.v 
    }
    else -> throw ArithmeticParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
   }
   return result
 }

 class First {
  var v: Int? = null
 }

 private fun first() : First {
   val result =  First()
   when (lexer.getToken()) {
    OP -> {
    val OP = check(ArithmeticToken.OP)
    val addSub = addSub()
    val CP = check(ArithmeticToken.CP)
    result.v = addSub.v
    }
    NUM -> {
    val NUM = check(ArithmeticToken.NUM)
    result.v = Integer.valueOf(NUM)
    }
    else -> throw ArithmeticParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
   }
   return result
 }

 class Fact {
  var v: Int? = null
 }

 private fun fact(p: Int?) : Fact {
   val result =  Fact()
   when (lexer.getToken()) {
    FAC -> {
    val FAC = check(ArithmeticToken.FAC)
     val pr = fac(p!!) 
    val fact = fact(pr)
     result.v = fact.v 
    }
    DIV, ADD, SUB, LOG, MUL, END, CP -> {
     result.v = p 
    }
    else -> throw ArithmeticParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
   }
   return result
 }

}
