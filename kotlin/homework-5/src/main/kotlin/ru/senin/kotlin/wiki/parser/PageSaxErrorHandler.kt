package ru.senin.kotlin.wiki.parser

import org.xml.sax.ErrorHandler
import org.xml.sax.SAXParseException
import ru.senin.kotlin.wiki.exception.WikiParserException

class PageSaxErrorHandler : ErrorHandler {
    override fun warning(exception: SAXParseException?) {
        if (exception != null) {
            println(exception.message)
        }
    }

    override fun error(exception: SAXParseException?) {
        throw WikiParserException("Fatal error, while parsing XML!")
    }

    override fun fatalError(exception: SAXParseException?) {
        throw WikiParserException("Fatal error, while parsing XML!")
    }
}