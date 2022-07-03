package ru.senin.kotlin.wiki

private val threads = 16
private val inputs = arrayOf(
    "ruwiki-20211101-pages-meta-current1.xml-p1p224167.bz2",
    "ruwiki-20211101-pages-meta-current2.xml-p224168p1042043.bz2",
    "ruwiki-20211101-pages-meta-current3.xml-p1042044p2198269.bz2",
    "ruwiki-20211101-pages-meta-current4.xml-p2198270p3698269.bz2",
    "ruwiki-20211101-pages-meta-current4.xml-p3698270p3835772.bz2",
    "ruwiki-20211101-pages-meta-current5.xml-p3835773p5335772.bz2",
    "ruwiki-20211101-pages-meta-current5.xml-p5335773p6585765.bz2",
    "ruwiki-20211101-pages-meta-current6.xml-p6585766p8085765.bz2",
    "ruwiki-20211101-pages-meta-current6.xml-p8085766p9047290.bz2"
)

fun main() {
    val args = arrayOf(
        "--threads", threads.toString(),
        "--inputs", inputs.map { "data/$it" }.joinToString(","),
        "--output", "results/threads_$threads.txt"
    )
    main(args)
}