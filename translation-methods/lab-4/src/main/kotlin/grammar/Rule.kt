package grammar

sealed interface Rule {
    val parent: String
}

sealed interface TerminalRule: Rule {
    val child: String
}

data class RegexRuleImpl(override val parent: String, override val child: String): TerminalRule
data class TerminalRuleImpl(override val parent: String, override val child: String): TerminalRule

data class NotTerminalRule(
    override val parent: String,
    val arguments: List<Argument>,
    val valueReturning: List<Argument>)
    : Rule
{
    val tokens: MutableList<List<RuleToken>> = ArrayList()

    fun addRule(list: List<RuleToken>) = tokens.add(list)

    fun getFields() : List<String> = valueReturning.map { "var ${it.name}: ${it.type}? = null" }
}