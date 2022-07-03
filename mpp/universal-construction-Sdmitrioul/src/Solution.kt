/**
 * @author :Skroba Dmitri
 */
class Solution : AtomicCounter {
    private val root = Node(0)
    private val last = ThreadLocal.withInitial { root }

    override fun getAndAdd(x: Int): Int {
        while (true) {
            val old = last.get().value
            val res = old + x

            val resNode = Node(res)
            val last = last.get().next.decide(resNode)

            this.last.set(last)

            if (last == resNode) {
                return old
            }
        }
    }

    private data class Node(val value: Int, val next: Consensus<Node> = Consensus())
}
