import printers.LL1Generator

const val DATA_PATH = "src/main/resources"

fun main(args: Array<String>) {
    LL1Generator(DATA_PATH, "arithmeticRes", "Arithmetic").generateAll()
    LL1Generator(DATA_PATH, "kotlinFunRes", "KotlinFun").generateAll()
}