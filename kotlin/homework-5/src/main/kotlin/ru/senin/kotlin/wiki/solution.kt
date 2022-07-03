package ru.senin.kotlin.wiki

import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream
import org.xml.sax.InputSource
import ru.senin.kotlin.wiki.model.Page
import ru.senin.kotlin.wiki.parser.PageSaxErrorHandler
import ru.senin.kotlin.wiki.parser.PageSaxHandler
import java.io.File
import java.lang.Thread.sleep
import java.util.concurrent.*
import javax.xml.parsers.SAXParserFactory

fun solve(parameters: Parameters) {
    checkInputsFiles(parameters)

    val title = ConcurrentHashMap<String, Int>()
    val words = ConcurrentHashMap<String, Int>()
    val articleSize = ConcurrentHashMap<Int, Int>()
    val years = ConcurrentHashMap<Int, Int>()

    val inputs = parameters.inputs
    val output = parameters.output
    val threads = parameters.threads

    handleFiles(inputs, threads, title, words, articleSize, years)

    writeResult(output, title, words, articleSize, years)
}

fun handleFiles(
    inputs: List<File>,
    threads: Int,
    title: ConcurrentHashMap<String, Int>,
    words: ConcurrentHashMap<String, Int>,
    articleSize: ConcurrentHashMap<Int, Int>,
    years: ConcurrentHashMap<Int, Int>
) {
    val queue = ConcurrentLinkedDeque<Page>()
    val files = ConcurrentLinkedDeque(inputs)
    val factory = SAXParserFactory.newInstance()
    val syncThreads = Phaser(1)
    val readSync = Phaser(inputs.size + 1)
    val executors = Executors.newFixedThreadPool(threads)

    repeat(threads) {
        syncThreads.register()
        executors.submit { createTask(factory, syncThreads, readSync, queue, files, title, words, articleSize, years) }
    }

    readSync.arriveAndAwaitAdvance()

    syncThreads.arriveAndAwaitAdvance()

    shutdownAndAwaitTermination(executors)
}

fun createTask(
    factory: SAXParserFactory,
    syncThreads: Phaser,
    readSync: Phaser,
    queue: ConcurrentLinkedDeque<Page>,
    files: ConcurrentLinkedDeque<File>,
    title: ConcurrentHashMap<String, Int>,
    words: ConcurrentHashMap<String, Int>,
    articleSize: ConcurrentHashMap<Int, Int>,
    years: ConcurrentHashMap<Int, Int>
) {
    while (files.isNotEmpty()) {
        val file = files.pollFirst()

        file ?: continue

        file.inputStream().buffered().use { inputStream ->
            try {
                BZip2CompressorInputStream(inputStream).use {
                    val xmlReader = factory.newSAXParser().xmlReader

                    xmlReader.errorHandler = PageSaxErrorHandler()
                    xmlReader.contentHandler = PageSaxHandler(
                        queue
                    )

                    xmlReader.parse(InputSource(it))
                }
            } catch (e: Exception) {
                //Ignore
            } finally {
                readSync.arrive()
            }
        }
    }

    while (queue.isNotEmpty() || readSync.phase != 1) {
        val page = queue.pollFirst()

        if (page == null) {
            sleep(200)
            continue
        }

        insertAllIn(processText(page.title), title)
        insertAllIn(processText(page.text), words)

        articleSize.merge(getSizeInterval(page.size), 1) { prev, _ -> prev + 1 }
        years.merge(page.timestamp, 1) { prev, _ -> prev + 1 }
    }

    syncThreads.arrive()
}


fun insertAllIn(words: Map<String, Int>, result: ConcurrentHashMap<String, Int>) {
    words.forEach { (word, count) -> result.merge(word, count) { prev, one -> prev + one } }
}

fun processText(text: String): Map<String, Int> {
    val regex = "[а-яА-Я]{3,}".toRegex()

    val result = mutableMapOf<String, Int>()

    regex.findAll(text).map { it.value.lowercase() }.forEach { word -> result.merge(word, 1) { old, _ -> old + 1 } }

    return result
}

fun getSizeInterval(size: Int): Int {
    return size.toString().length - 1
}

fun shutdownAndAwaitTermination(pool: ExecutorService) {
    pool.shutdown()
    try {
        if (!pool.awaitTermination(60, TimeUnit.SECONDS)) {
            pool.shutdownNow()
            if (!pool.awaitTermination(60, TimeUnit.SECONDS))
                System.err.println("Pool did not terminate")
        }
    } catch (ie: InterruptedException) {
        pool.shutdownNow();
        Thread.currentThread().interrupt();
    }
}
