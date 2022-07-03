import org.junit.jupiter.api.Test

internal class SimpleTests {
    @Test
    fun `test fun without anything`() {
        abstractTest("fun my()")
    }

    @Test
    fun `test fun with returning type`() {
        abstractTest("fun my(): Type")
    }

    @Test
    fun `test fun with one arg`() {
        abstractTest("fun my(arg: Typer): Type")
    }

    @Test
    fun `test fun with two args`() {
        abstractTest("fun my(arg: Typer, arg: Typer): Type")
    }

    @Test
    fun `test fun with one arg without fun type`() {
        abstractTest("fun my(arg: Typer)")
    }

    @Test
    fun `test fun with two args without fun type`() {
        abstractTest("fun my(arg: Typer, arg: Typer)")
    }
}