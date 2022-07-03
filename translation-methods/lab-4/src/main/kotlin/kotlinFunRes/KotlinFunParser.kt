package kotlinFunRes

import kotlinFunRes.KotlinFunToken.*
import  guru.nidi.graphviz.model.Factory.* 
import  guru.nidi.graphviz.model.LinkSource 
import  util.Incrementor 
import  util.ListOfSource 

class KotlinFunParser(private val input: String) {
 private val lexer = KotlinFunLexer(input)

 private fun check(token: KotlinFunToken) : String {
  if (lexer.getToken() != token) {
   throw KotlinFunParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
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
   throw KotlinFunParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
  }
  return result
 }
 class Run {
  var res: ListOfSource? = null
 }

 private fun run() : Run {
   val result =  Run()
   when (lexer.getToken()) {
    FUN -> {
     result.res = ListOfSource() 
     val inc = Incrementor() 
     inc.inc() 
     val s = "№ " + inc.value + "\nS" 
     inc.inc() 
    val FUN = check(KotlinFunToken.FUN)
     result.res!!.add(node(s).link(node("№ " + inc.value + "\n" + FUN))) 
     inc.inc() 
    val NAME = check(KotlinFunToken.NAME)
     result.res!!.add(node(s).link(node("№ " + inc.value + "\n" + NAME))) 
     inc.inc() 
    val OP = check(KotlinFunToken.OP)
     result.res!!.add(node(s).link(node("№ " + inc.value + "\n" + OP))) 
     inc.inc() 
     result.res!!.add(node(s).link(node("№ " + inc.value + "\nARGS"))) 
    val args = args( inc ,  result.res ,  "№ " + inc.value + "\nARGS" )
     inc.inc() 
    val CP = check(KotlinFunToken.CP)
     result.res!!.add(node(s).link(node("№ " + inc.value + "\n" + CP))) 
     inc.inc() 
     result.res!!.add(node(s).link(node("№ " + inc.value + "\nRetType"))) 
    val retType = retType( inc ,  result.res ,  "№ " + inc.value + "\nRetType" )
    val END = check(KotlinFunToken.END)
    }
    else -> throw KotlinFunParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
   }
   return result
 }

 class Args {
 }

 private fun args(inc: Incrementor?, res: ListOfSource?, parent: String?) : Args {
   val result =  Args()
   when (lexer.getToken()) {
    NAME -> {
    val NAME = check(KotlinFunToken.NAME)
     inc!!.inc() 
     res!!.add(node(parent).link(node("№ " + inc.value + "\n" + NAME))) 
    val SEMICOLON = check(KotlinFunToken.SEMICOLON)
     inc!!.inc() 
     res!!.add(node(parent).link(node("№ " + inc.value + "\n:"))) 
    val OBJ = check(KotlinFunToken.OBJ)
     inc!!.inc() 
     res!!.add(node(parent).link(node("№ " + inc.value + "\n" + OBJ))) 
     inc!!.inc() 
     res!!.add(node(parent).link(node("№ " + inc.value + "\nARGSR"))) 
    val argsr = argsr( inc ,  res ,  "№ " + inc.value + "\nARGSR" )
    }
    CP -> {
     inc!!.inc() 
     res!!.add(node(parent).link(node("№ " + inc.value + "\nEPS"))) 
    }
    else -> throw KotlinFunParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
   }
   return result
 }

 class Argsr {
 }

 private fun argsr(inc: Incrementor?, res: ListOfSource?, parent: String?) : Argsr {
   val result =  Argsr()
   when (lexer.getToken()) {
    COMMA -> {
    val COMMA = check(KotlinFunToken.COMMA)
     inc!!.inc() 
     res!!.add(node(parent).link(node("№ " + inc.value + "\n,"))) 
    val NAME = check(KotlinFunToken.NAME)
     inc!!.inc() 
     res!!.add(node(parent).link(node("№ " + inc.value + "\n" + NAME))) 
    val SEMICOLON = check(KotlinFunToken.SEMICOLON)
     inc!!.inc() 
     res!!.add(node(parent).link(node("№ " + inc.value + "\n:"))) 
    val OBJ = check(KotlinFunToken.OBJ)
     inc!!.inc() 
     res!!.add(node(parent).link(node("№ " + inc.value + "\n" + OBJ))) 
     inc!!.inc() 
     res!!.add(node(parent).link(node("№ " + inc.value + "\nARGSR"))) 
    val argsr = argsr( inc ,  res ,  "№ " + inc.value + "\nARGSR" )
    }
    CP -> {
     inc!!.inc() 
     res!!.add(node(parent).link(node("№ " + inc.value + "\nEPS"))) 
    }
    else -> throw KotlinFunParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
   }
   return result
 }

 class RetType {
 }

 private fun retType(inc: Incrementor?, res: ListOfSource?, parent: String?) : RetType {
   val result =  RetType()
   when (lexer.getToken()) {
    SEMICOLON -> {
    val SEMICOLON = check(KotlinFunToken.SEMICOLON)
     inc!!.inc() 
     res!!.add(node(parent).link(node("№ " + inc.value + "\n:"))) 
    val OBJ = check(KotlinFunToken.OBJ)
     inc!!.inc() 
     res!!.add(node(parent).link(node("№ " + inc.value + "\n" + OBJ))) 
    }
    END -> {
     inc!!.inc() 
     res!!.add(node(parent).link(node("№ " + inc.value + "\nEPS"))) 
    }
    else -> throw KotlinFunParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
   }
   return result
 }

}
