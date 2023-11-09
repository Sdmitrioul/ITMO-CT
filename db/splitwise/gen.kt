fun main() {
}

fun buildClosure (s: Set<Char>):Set<Char> {

    val closure = mutableListOf<Char>()
    var changed = true

    while (changed) {
        changed = false
        for (f in closure) {
            for (rule in rules) {    //rules - правила вывода
                var new_f = rule.apply(f, closure)

                changed =
                    closure.add(new_f)    //add - возвращает true, если элемент был добавлен, false - иначе
            }
        }
    }

    return closure
}