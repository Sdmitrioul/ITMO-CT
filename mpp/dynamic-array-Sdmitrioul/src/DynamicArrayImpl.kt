import kotlinx.atomicfu.AtomicBoolean
import kotlinx.atomicfu.atomic
import kotlinx.atomicfu.atomicArrayOfNulls

class DynamicArrayImpl<E> : DynamicArray<E> {
    private val core = atomic(Core<E>(INITIAL_CAPACITY))
    private val isValidCore: AtomicBoolean = atomic(false)

    override fun get(index: Int): E {
        checkIndex(index >= 0, "Index must be positive")
        checkIndex(index < size, "Index bigger than size")

        val node = core.value.array[index].value

        check(node != null){ "Can't be null" }

        return node.value
    }

    override fun put(index: Int, element: E) {
        checkIndex(index >= 0, "Index must be positive")
        checkIndex(index < size, "Index bigger than size")

        while (true) {
            val node =  core.value.array[index].value

            check(node != null){ "Can't be null" }

            if (!node.isValid) {
                continue
            }

            if (core.value.array[index].compareAndSet(node, Node(element))) {
                return
            }
        }
    }

    override fun pushBack(element: E) {
        val newNode =  Node(element)

        while (true) {
            val core = core.value
            val size = core.size.value

            if (size == core.capacity) {
                ensureCapacity(core, size)
                continue
            }

            if (core.array[size].compareAndSet(null, newNode)) {
                core.size.incrementAndGet()
                return
            }
        }
    }

    override val size: Int
        get() {
            return core.value.size.value
        }

    private fun ensureCapacity(core : Core<E>, staticSize : Int) {
        if (!isValidCore.compareAndSet(expect = false, update = true)) {
            return
        }

        val newCore = Core<E>(core.capacity * 2 + 1)

        for (index in 0 until staticSize) {
            insertNode(core, newCore, index)
        }

        newCore.size.compareAndSet(0, staticSize)
        this.core.compareAndSet(core, newCore)
        isValidCore.compareAndSet(expect = true, update = false)
    }

    private fun insertNode(oldCore: Core<E>, newCore: Core<E>, index: Int) {
        while (true) {
            val node = oldCore.array[index].value

            check(node != null){ "Can't be null" }

            val newNode = Node(node.value, false)

            if (oldCore.array[index].compareAndSet(node, newNode)) {
                newCore.array[index].compareAndSet(null, node)
                break
            }
        }
    }

    private fun checkIndex(condition: Boolean, message: String) {
        if (!condition) {
            throw IllegalArgumentException(message)
        }
    }
}

data class Node<E>(val value: E, val isValid: Boolean = true)

class Core<E>(
    val capacity: Int
) {
    val size = atomic(0)
    val array = atomicArrayOfNulls<Node<E>>(capacity)
}

private const val INITIAL_CAPACITY = 1 // DO NOT CHANGE ME