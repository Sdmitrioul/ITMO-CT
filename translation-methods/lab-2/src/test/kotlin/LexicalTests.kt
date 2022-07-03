import org.junit.jupiter.api.Test
import org.junit.jupiter.api.assertThrows
import ru.itmo.m3334.skroba.tokenizer.TokenizerException

class LexicalTests {
    @Test
    fun `test with a lot of spaces`() {
        val value =  "fun     my(     )    "
        abstractTest(value, value.removeDoubleSpaces())
    }

    @Test
    fun `test fun with returning type, with a lot of spaces`() {
        val value = "fun   my(      )   :      Type   "
        abstractTest(value, value.removeDoubleSpaces())
    }

    @Test
    fun `test fun with one arg, with a lot of spaces`() {
        val value = "fun     my(  arg   :   Typer  )   :    Type    "
        abstractTest(value, value.removeDoubleSpaces())
    }

    @Test
    fun `test fun with two args, with a lot of spaces`() {
        val value = "fun    my(   arg   :   Typer   ,    arg  :    Typer  )  :   Type   "
        abstractTest(value, value.removeDoubleSpaces())
    }

    @Test
    fun `test fun with one arg without fun type, with a lot of spaces`() {
        val value = "fun     my(arg    :     Typer   )     "
        abstractTest(value, value.removeDoubleSpaces())
    }

    @Test
    fun `test fun with two args without fun type, with a lot of spaces`() {
        val value = "fun    my(     arg   :   Typer  ,   arg   :   Typer     )   "
        abstractTest(value, value.removeDoubleSpaces())
    }

    @Test
    fun `test failed with wrong character`() {
        val first = "fun *()"
        val second = "fun ^$#():Ty"
        assertThrows<TokenizerException> { parse(first) }
        assertThrows<TokenizerException> { parse(second) }
    }

    private fun String.removeDoubleSpaces(): String {
        return this.replace("\\s+".toRegex(), " ")
    }
}