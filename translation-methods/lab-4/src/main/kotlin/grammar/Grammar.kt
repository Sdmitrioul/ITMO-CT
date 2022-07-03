package grammar

class Grammar(val start: String) {
    val terminalsRules: MutableList<TerminalRule> = ArrayList()
    val notTerminalsRules: MutableList<NotTerminalRule> = ArrayList()

    val first: MutableMap<String, MutableSet<String>> = HashMap()
    val follow: MutableMap<String, MutableSet<String>> = HashMap()

    var imports: MutableList<String> = ArrayList()

    fun addTerminalRule(rule: TerminalRule) = terminalsRules.add(rule)

    fun addNotTerminalRule(rule: NotTerminalRule) = notTerminalsRules.add(rule)

    fun buildAll() {
        buildFirstSet()
        buildFollowSet()
    }

    fun firstSetByRP(tokens: List<RuleToken>) : Set<String> {
        tokens.filter { it is Terminal || it is NotTerminal }.forEach {
            return if (it is Terminal) {
                setOf(it.name)
            } else {
                first[it.name]?.toHashSet() ?: setOf()
            }
        }

        return setOf(EPS)
    }

    private fun init(map: MutableMap<String, MutableSet<String>>) {
        if (map.isNotEmpty()) return

        notTerminalsRules.forEach {
            map[it.parent] = HashSet()
        }
    }

    private fun buildFirstSet() {
        init(first)

        var changed = true

        while (changed) {
            changed = false

            notTerminalsRules.forEach {
                it.tokens.forEach { rightPart ->
                    changed = changed || first[it.parent]?.addAll(firstSetByRP(rightPart)) ?: changed
                }
            }
        }
    }

    private fun buildFollowSet() {
        init(follow)

        follow[start]?.add(END)

        var changed = true

        while (changed) {
            changed = false

            notTerminalsRules.forEach {
                it.tokens.forEach { rightPart ->
                    rightPart.forEachIndexed { index, ruleToken ->
                        if (ruleToken is NotTerminal) {
                            val set = HashSet<String>()
                            set.addAll(firstSetByRP(rightPart.subList(index + 1, rightPart.size)))

                            if (set.remove(EPS)) {
                                follow[it.parent]?.let { it1 -> set.addAll(it1) }
                            }

                            changed = changed || follow[ruleToken.name]?.addAll(set) ?: false
                        }
                    }
                }
            }
        }
    }

    fun checkLL1(): Boolean {
        notTerminalsRules.forEach {
            val setF = HashSet<String>()
            it.tokens.forEach { right ->
                var flag = true

                for (token in right.filter { t -> t !is Code }) {
                    var set: MutableSet<String>? = null

                    if (token is Terminal) {
                        set = hashSetOf(token.name)
                    } else if (token is NotTerminal) {
                        set = first[token.name]

                        if (set?.contains(EPS) == true) {
                            set.remove(EPS)
                            flag = false
                        }
                    }

                    if (set != null) {
                        if (set.isNotEmpty()) {
                            if (setF.isEmpty()) {
                                setF.addAll(set)
                            } else {
                                if (set.intersect(setF).isNotEmpty()) {
                                    return false
                                }

                                setF.addAll(set)
                            }

                            if (flag) break
                        }
                    }
                }
                if (!flag) {
                    setF.add(EPS)
                }
            }
        }

        return true
    }

    companion object {
        const val EPS = "ε"
        const val END = "$"
    }
}