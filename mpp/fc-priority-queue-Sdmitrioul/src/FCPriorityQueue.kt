import kotlinx.atomicfu.AtomicBoolean
import kotlinx.atomicfu.AtomicRef
import kotlinx.atomicfu.atomic
import kotlinx.atomicfu.atomicArrayOfNulls
import java.util.*

class FCPriorityQueue<E : Comparable<E>> {
    private val capacity = 17
    private val queue = PriorityQueue<E>()
    private val lock = atomic(false)
    private val operations = atomicArrayOfNulls<Operation<E>>(capacity)

    /**
     * Retrieves the element with the highest priority
     * and returns it as the result of this function;
     * returns `null` if the queue is empty.
     */
    fun poll(): E? {
        return processOperation(Operation(OType.POLL))
    }

    /**
     * Returns the element with the highest priority
     * or `null` if the queue is empty.
     */
    fun peek(): E? {
        return processOperation(Operation(OType.PEEK))
    }

    /**
     * Adds the specified element to the queue.
     */
    fun add(element: E) {
        processOperation(Operation(OType.ADD, element))
    }

    private fun processOperation(operation: Operation<E>): E? {
        var index: Int = DUMMY

        while (true) {
            if (lock.compareAndSet(expect = false, update = true)) {
                try {
                    return processMain(index, operation)
                } finally {
                    lock.compareAndSet(expect = true, update = false)
                }
            }

            if (index != DUMMY) {
                if (operation.isReady.value) {
                    operations[index].compareAndSet(operation, null)
                    return operation.result.value
                }

                continue
            }

            for (i in 0 until capacity) {
                if (!operations[i].compareAndSet(null, operation)) {
                    continue
                }

                index = i
                break
            }
        }
    }

    private fun evaluate(operation: Operation<E>) {
        when (operation.type) {
            OType.PEEK -> {
                operation.result.compareAndSet(null, queue.peek())
            }
            OType.ADD -> {
                queue.add(operation.value)
            }
            OType.POLL -> {
                operation.result.compareAndSet(null, queue.poll())
            }
        }
        operation.isReady.compareAndSet(expect = false, update = true)
    }

    private fun processMain(index: Int, operation: Operation<E>): E? {
        for (i in 0 until capacity) {
            val op = operations[i].value ?: continue

            if (op.isReady.value) continue

            evaluate(op)
        }

        if (index == DUMMY) {
            evaluate(operation)
            return operation.result.value
        }

        val result = operations[index].value ?:  return null

        operations[index].compareAndSet(result, null)

        return result.result.value
    }

    private class Operation<E>(
        val type: OType,
        val value: E? = null,
        val isReady: AtomicBoolean = atomic(false),
        val result: AtomicRef<E?> = atomic(null)
    )

    private enum class OType {
        PEEK, ADD, POLL
    }
}

private const val DUMMY = -1