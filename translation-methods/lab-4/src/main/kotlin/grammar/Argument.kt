package grammar

data class Argument(
val name: String,
val type: String
) {
    override fun toString(): String {
        return "${name}: $type?"
    }
}
