package ru.senin.kotlin.wiki

import java.io.File
import java.io.FileOutputStream
import java.nio.file.Files

fun checkParentDirectory(file: File) {
    if (file.parent != null && !Files.exists(file.parentFile.toPath())) {
        Files.createDirectories(file.parentFile.toPath())
    }
}

fun writeResult(
    outputFileName: String,
    titles: Map<String, Int>,
    words: Map<String, Int>,
    articleSize: Map<Int, Int>,
    yearsDep: Map<Int, Int>
) {
    val title = "Топ-300 слов в заголовках статей:\n"
    val article = "Топ-300 слов в статьях:\n"
    val size = "Распределение статей по размеру:\n"
    val year = "Распределение статей по времени:\n"

    val outputFile = File(outputFileName)

    checkParentDirectory(outputFile)

    outputFile.outputStream().use {
        writeTopOf(it, titles, title)

        writeTopOf(it, words, article)

        writeInfo(it, articleSize, size)

        it.write("\n".toByteArray())

        writeInfo(it, yearsDep, year)
    }
}

fun writeInfo(
    writer: FileOutputStream,
    data: Map<Int, Int>,
    title: String
) {
    writer.write(title.toByteArray())

    val min = data.minByOrNull { (key, _) -> key }?.key ?: -1
    val max = data.maxByOrNull { (key, _) -> key }?.key ?: -1

    if (min == -1) {
        return
    }

    for (i in min..max) {
        val value = data.getOrDefault(i, 0)
        writer.write("$i $value\n".toByteArray())
    }
}

private fun writeTopOf(
    writer: FileOutputStream,
    data: Map<String, Int>,
    title: String,
) {
    val count = 300
    val newLine = "\n"

    writer.write(title.toByteArray())
    data.toList().sortedWith(
        compareByDescending<Pair<String, Int>> { it.second }.thenBy { it.first }
    ).take(count)
        .forEach { (word, count) ->
            writer.write("$count $word\n".toByteArray())
        }
    writer.write(newLine.toByteArray())
}


