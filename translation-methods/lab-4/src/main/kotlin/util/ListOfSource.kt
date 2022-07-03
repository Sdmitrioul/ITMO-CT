package util

import guru.nidi.graphviz.model.LinkSource

class ListOfSource {
    val list : MutableList<LinkSource> = ArrayList()

    fun add(element: LinkSource) = list.add(element)
}