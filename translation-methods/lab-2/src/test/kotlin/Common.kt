import org.junit.jupiter.api.Assertions.assertEquals
import ru.itmo.m3334.skroba.parser.KotlinFunParser
import ru.itmo.m3334.skroba.parser.Tree

fun parse(input: String): Tree {
    return KotlinFunParser(input).parse()
}

fun abstractTest(actual: String, waited: String = actual) {
    assertEquals(waited, parse(actual).toString())
}