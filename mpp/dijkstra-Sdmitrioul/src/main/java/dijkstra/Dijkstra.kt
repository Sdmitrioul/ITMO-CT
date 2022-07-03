package dijkstra

import java.util.*
import java.util.concurrent.Phaser
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.locks.ReentrantLock

import kotlin.Comparator
import kotlin.concurrent.thread
import kotlin.random.Random

private val NODE_DISTANCE_COMPARATOR = Comparator<Node> { o1, o2 -> o1!!.distance.compareTo(o2!!.distance) }

fun shortestPathParallel(start: Node) {
    val workers = Runtime.getRuntime().availableProcessors()

    start.distance = 0

    val onFinish = Phaser(workers + 1)
    val queue = MultiQueue(2 * workers + 1, NODE_DISTANCE_COMPARATOR)

    queue.add(start)

    repeat(workers) {
        thread {
            while (true) {
                if (queue.isEmpty()) {
                    break
                }

                val cur: Node = queue.poll() ?: continue

                for (e in cur.outgoingEdges) {
                    while (true) {
                        val oldDist = e.to.distance
                        val newDist = cur.distance + e.weight


                        if (oldDist <= newDist) break

                        if (e.to.casDistance(oldDist, newDist)) {
                            queue.add(e.to)

                            break
                        }
                    }
                }

                queue.decrement()
            }
            onFinish.arrive()
        }
    }
    onFinish.arriveAndAwaitAdvance()
}

class MultiQueue<T>(private val dimension: Int, private val comparator: Comparator<T>) {
    private val queues = Array(dimension) { PQWithLock(ReentrantLock(), PriorityQueue( comparator)) }
    private val _size = AtomicInteger(0)

    fun add(element: T) {
        _size.incrementAndGet()

        while (true) {
            val queue = randomQueue()

            if (!queue.lock.tryLock()) continue

            try {
                queue.queue.add(element)
            } finally {
                queue.lock.unlock()
                break
            }
        }
    }

    fun poll(): T? {
        while (true) {
            val firstQueue = randomQueue()
            val secondQueue = randomQueue()

            if (!firstQueue.lock.tryLock()) continue

            try {
                if (!secondQueue.lock.tryLock()) continue

                try {
                    val first = firstQueue.queue.peek() ?: return secondQueue.queue.poll()
                    val second = secondQueue.queue.peek() ?: return firstQueue.queue.poll()

                    return when {
                        comparator.compare(first, second) > 0 -> secondQueue.queue.poll()
                        else -> firstQueue.queue.poll()
                    }
                } finally {
                    secondQueue.lock.unlock()
                }
            } finally {
                firstQueue.lock.unlock()
            }
        }
    }

    private fun randomQueue() = queues[Random.nextInt(dimension)]

    fun isEmpty(): Boolean {
        return _size.get() <= 0
    }

    fun decrement() {
        _size.decrementAndGet()
    }
}

data class PQWithLock<T>(val lock: ReentrantLock, val queue: PriorityQueue<T>)