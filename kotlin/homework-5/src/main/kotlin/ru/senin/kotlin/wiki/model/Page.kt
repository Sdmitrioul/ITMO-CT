package ru.senin.kotlin.wiki.model

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException

data class Page(val title: String, val text: String, var size: Int, var timestamp: Int)

class PageBuilder {
    var title: String? = null
    var text: String? = null
    var size: String? = null
    var timestamp: String? = null

    fun getPage(): Page? {
        val title = title
        val text = text
        val size = size?.toIntOrNull()
        val timestamp: Int?

        try {
            val format = DateTimeFormatter.ISO_OFFSET_DATE_TIME
            timestamp = LocalDateTime.parse(this.timestamp, format).year
        } catch (e: DateTimeParseException) {
            println(e.message)
            return null
        }

        eraseInfo()

        if (title == null || text == null || size == null) return null

        return Page(title, text, size, timestamp)
    }

    private fun eraseInfo() {
        title = null
        text = null
        size = null
        timestamp = null
    }
}
