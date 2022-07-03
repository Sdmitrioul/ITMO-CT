import kotlinx.atomicfu.*

class FAAQueue<T> {
    private val head: AtomicRef<Segment> // Head pointer, similarly to the Michael-Scott queue (but the first node is _not_ sentinel)
    private val tail: AtomicRef<Segment> // Tail pointer, similarly to the Michael-Scott queue

    init {
        val firstNode = Segment()
        head = atomic(firstNode)
        tail = atomic(firstNode)
    }

    /**
     * Adds the specified element [x] to the queue.
     */
    fun enqueue(x: T) {
        while (true) {
            val tail = tail.value
            val enqIndex = tail.enqIdx.getAndIncrement()

            if (enqIndex < SEGMENT_SIZE && tail.elements[enqIndex].compareAndSet(null, x)) {
                return
            }

            val segment = Segment(x)

            if (tail.next.compareAndSet(null, segment)) {
                this.tail.compareAndSet(tail, segment)
                return
            }

            val next = tail.next.value

            check(next != null) { "Impossible state" }

            this.tail.compareAndSet(tail, next)
        }
    }

    /**
     * Retrieves the first element from the queue
     * and returns it; returns `null` if the queue
     * is empty.
     */
    fun dequeue(): T? {
        while (true) {
            val head = head.value
            val deqIndex = head.deqIdx.getAndIncrement()

            if (deqIndex >= SEGMENT_SIZE) {
                val nextHead = head.next.value ?: return null

                this.head.compareAndSet(head, nextHead)
                continue
            }

            val res = head.elements[deqIndex].getAndSet(DONE) ?: continue

            @Suppress("UNCHECKED_CAST")
            return res as T
        }
    }

    /**
     * Returns `true` if this queue is empty;
     * `false` otherwise.
     */
    val isEmpty: Boolean get() {
        while (true) {
            val headCurrent = head.value
            if (!headCurrent.isEmpty) {
                return false
            }

            val nextSegment = headCurrent.next.value ?: return true

            head.compareAndSet(headCurrent, nextSegment)
        }
    }
}

private class Segment {
    val next: AtomicRef<Segment?> = atomic(null)
    val enqIdx = atomic(0) // index for the next enqueue operation
    val deqIdx = atomic(0) // index for the next dequeue operation
    val elements = atomicArrayOfNulls<Any>(SEGMENT_SIZE)

    constructor() // for the first segment creation

    constructor(x: Any?) { // each next new segment should be constructed with an element
        enqIdx.incrementAndGet()
        elements[0].compareAndSet(null, x)
    }

    val isEmpty: Boolean get() = deqIdx.value >= enqIdx.value || deqIdx.value >= SEGMENT_SIZE

}

private val DONE = Any() // Marker for the "DONE" slot state; to avoid memory leaks
const val SEGMENT_SIZE = 2 // DO NOT CHANGE, IMPORTANT FOR TESTS

