package ru.senin.kotlin.wiki.parser

import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler
import ru.senin.kotlin.wiki.model.Page
import ru.senin.kotlin.wiki.model.PageBuilder
import java.util.concurrent.ConcurrentLinkedDeque

class PageSaxHandler(private val queue: ConcurrentLinkedDeque<Page>) : DefaultHandler() {
    private val text = StringBuilder()
    private val pageBuilder = PageBuilder()
    private val position = arrayListOf<String>()

    override fun startElement(uri: String?, localName: String?, qName: String?, attributes: Attributes?) {
        if (qName == null) {
            return
        }

        if ((position.isEmpty() && qName == PAGE) || (position.isNotEmpty() && position[0] == PAGE)) {
            position.add(qName)
        }

        if (qName.lowercase() == TEXT && curPosition() == TEXT_URI) {
            pageBuilder.size = attributes?.getValue("bytes")
        }
    }

    override fun endElement(uri: String?, localName: String?, qName: String?) {
        if (qName == null) {
            return
        }

        if (qName.lowercase() == TITLE && curPosition() == TITLE_URI) {
            pageBuilder.title = text.toString().trim()
        }

        if (qName.lowercase() == TEXT && curPosition() == TEXT_URI) {
            pageBuilder.text = text.toString().trim()
        }

        if (qName.lowercase() == TIME && curPosition() == TIME_URI) {
            pageBuilder.timestamp = text.toString().trim()
        }

        if (qName.lowercase() == PAGE) {
            val page = pageBuilder.getPage()
            page?.let { queue.add(it) }
        }

        text.clear()

        if (position.isNotEmpty()) {
            position.removeLast()
        }
    }

    override fun characters(ch: CharArray?, start: Int, length: Int) {
        text.append(ch, start, length)
    }

    private fun curPosition(): String {
        return position.joinToString(SEP)
    }

    companion object {
        const val SEP = "/"
        const val PAGE = "page"
        const val TITLE = "title"
        const val TEXT = "text"
        const val TIME = "timestamp"
        const val TITLE_URI = "page/title"
        const val TEXT_URI = "page/revision/text"
        const val TIME_URI = "page/revision/timestamp"
    }
}