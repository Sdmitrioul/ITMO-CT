package ru.senin.kotlin.wiki

import ru.senin.kotlin.wiki.exception.WikiArgumentException
import java.io.File

fun checkInputsFiles(parameters: Parameters) {
    if (!checkInputs(parameters.inputs)) {
        throw WikiArgumentException("Wrong format of inputs file, waited \".bz2\"")
    }
}

fun checkInputs(files: List<File>): Boolean {
    return files.all { file -> file.extension == "bz2" }
}