import arithmeticRes.ArithmeticParser
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class TestArichmetic {
    @Test
    fun testNum() {
        val input = "90"
        assertEquals(ArithmeticParser(input).parse().v, 90)
    }

    @Test
    fun testSum() {
        val input = "1 + 50"
        assertEquals(ArithmeticParser(input).parse().v, 51)
    }

    @Test
    fun testMinus() {
        val input = "1 - 50"
        assertEquals(ArithmeticParser(input).parse().v, -49)
    }

    @Test
    fun testMul() {
        val input = "1 * 50"
        assertEquals(ArithmeticParser(input).parse().v, 50)
    }

    @Test
    fun testDev() {
        val input = "1 / 50"
        assertEquals(ArithmeticParser(input).parse().v, 0)
    }

    @Test
    fun testRandom() {
        val input = "1 * (4 + 1) - 34 / 2"
        assertEquals(ArithmeticParser(input).parse().v, -12)
    }

    @Test
    fun testFactorial() {
        val input = "5!"
        assertEquals(ArithmeticParser(input).parse().v, 120)
    }

    @Test
    fun testFactorialHardI() {
        val input = "5! * (1 + 3)!"
        assertEquals(ArithmeticParser(input).parse().v, 2880)
        val input2 = "5! - (1 + 3)!"
        assertEquals(ArithmeticParser(input2).parse().v, 96)
        val input3 = "3!!"
        assertEquals(ArithmeticParser(input3).parse().v, 720)
    }
}