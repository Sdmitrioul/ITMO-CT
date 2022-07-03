package printers

import printers.BasePrinter.Companion.NL
import java.io.BufferedWriter

class ExceptionPrinter(override val grammarName: String, private val exceptionName: String, override val path: String): BasePrinter {
    override val fileName = "${grammarName}${exceptionName}Exception"

    override fun generateInner(printer: BufferedWriter) {
        printer.write("import java.text.ParseException$NL$NL")

        printer.write("class ${fileName}(override val message: String, errorOffset: Int) : ParseException(message, errorOffset)$NL")
    }
}