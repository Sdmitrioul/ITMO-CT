import java.util.concurrent.atomic.AtomicReference

class Solution(val env: Environment) : Lock<Solution.Node> {
    val tail: AtomicReference<Node> = AtomicReference(null)

    override fun lock(): Node {
        val my = Node()
        my.locked.compareAndSet(false, true)

        val prev = tail.getAndSet(my) ?: return my

        prev.next.set(my)

        while (my.locked.get()) env.park()

        return my
    }

    override fun unlock(node: Node) {
        if (node.next.get() == null) {
            if (tail.compareAndSet(node, null)) {
                return
            }

            while (node.next.get() == null) {}
        }

        node.next.get().locked.set(false)
        env.unpark(node.next.get().thread)
    }

    class Node {
        val thread = Thread.currentThread() // запоминаем поток, которые создал узел
        val locked = AtomicReference(false)
        val next: AtomicReference<Node> = AtomicReference()
    }
}