package grammar

interface RuleToken {
    val name: String
}

data class NotTerminal(override val name: String) : RuleToken {
    val args: MutableList<String> = ArrayList()

    fun addArg(arg: String): Boolean = args.add(arg)
}

data class Terminal(override val name: String) : RuleToken

data class Code(override val name: String) : RuleToken {
    val code: String
        get() {
            return name.replace("$", "result.")
        }
}