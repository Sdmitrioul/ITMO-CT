package printers

import java.io.BufferedWriter
import java.io.File
import java.io.Writer
import java.nio.file.Paths
import kotlin.io.path.createDirectories
import kotlin.io.path.notExists

interface BasePrinter {
    val grammarName: String
    val fileName: String
    val path: String
    fun generateInner(printer: BufferedWriter)

    fun generate() {
        val pathGl = Paths.get("src/main/kotlin/$path").resolve("$fileName.kt")

        if (pathGl.parent.notExists()) {
            pathGl.parent.createDirectories()
        }

        val file = File(pathGl.toString())

        file.createNewFile()

        file.bufferedWriter().use {
            if (file.parent != null) {
                it.write("package $path$NL$NL")
            }

            generateInner(it)
        }
    }

    companion object {
        const val NL = "\n"
        const val WS = " "
        private const val TAB = "    "

        fun print(printer: Writer, line: String, tabsCount: Int = 1) {
            printer.write("${TAB.take(tabsCount)}$line$NL")
        }
    }
}