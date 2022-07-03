package dijkstra

import dijkstra.messages.*
import dijkstra.system.environment.Environment

class ProcessImpl(private val environment: Environment) : Process {
    private var result: Long? = null
    private var waiting: Int = 0
    private var parentId: Int = -1
    private var children: Int = 0

    private val isUnparent: Boolean
        get() = parentId == -1

    override fun onMessage(srcId: Int, message: Message) {
        if (message !is DijkstraMessage) {
            return
        }

        when (message.type) {
            MessageType.FINISH -> children--
            MessageType.REFUSE -> waiting--
            MessageType.OK -> {
                waiting--
                children++
            }
            MessageType.INFO -> {
                val distance = message.info ?: 0
                val localDist = result

                if (localDist != null && distance >= localDist) {
                    sendAnswer(srcId, false)

                    return
                }

                sendAnswer(srcId, isUnparent)
                parentId = if (isUnparent) srcId else parentId
                result = distance

                calculateDistances()

                return
            }
        }

        tryFinish()
    }

    override fun getDistance(): Long? {
        return result
    }

    override fun startComputation() {
        result = 0
        calculateDistances()
    }

    private fun calculateDistances() {
        environment.neighbours.forEach { sendDistance(it) }

        tryFinish()
    }

    private fun tryFinish() {
        if (waiting != 0 || children != 0) return

        if (isUnparent) {
            environment.finishExecution()
            return
        }

        environment.send(parentId, DijkstraMessage(MessageType.FINISH))
        parentId = -1
    }

    private fun sendDistance(it: Map.Entry<Int, Long>) {
        val distance: Long = (result ?: 0) + it.value

        waiting++
        environment.send(it.key, DijkstraMessage(MessageType.INFO, distance))
    }

    private fun sendAnswer(id: Int, ans: Boolean) {
        environment.send(id, DijkstraMessage(if (!ans) MessageType.REFUSE else MessageType.OK))
    }
}