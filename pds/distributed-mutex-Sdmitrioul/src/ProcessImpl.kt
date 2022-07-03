package mutex

/**
 * Distributed mutual exclusion implementation.
 * All functions are called from the single main thread.
 *
 * @author Дмитрий Скроба
 */
class ProcessImpl(private val env: Environment) : Process {
    private val countEdges: Int = env.nProcesses + 1
    private val forks: MutableList<ForkType> = MutableList(countEdges) { ForkType.DIRTY }
    private var requested: MutableList<Boolean> = MutableList(countEdges) { false }
    private var inCS: Boolean = false
    private var hungry: Boolean = false

    init {
        ((env.processId + 1) until countEdges).forEach { forks[it] = ForkType.ABSENT }
    }

    override fun onMessage(srcId: Int, message: Message) {
        message.parse {
            when (readEnum<MsgType>()) {
                MsgType.REQ -> {
                    if (inCS || forks[srcId] != ForkType.DIRTY) {
                        requested[srcId] = true
                        return
                    }

                    forks[srcId] = ForkType.ABSENT

                    if (hungry) send(srcId, MsgType.REQ)

                    send(srcId, MsgType.REL)
                }
                MsgType.REL -> {
                    forks[srcId] = ForkType.CLEAN
                    checkCSEnter()
                }
            }
        }
    }

    override fun onLockRequest() {
        hungry = true

        if (checkCSEnter()) return

        forks.indices.filter { forks[it] == ForkType.ABSENT }.forEach { send(it, MsgType.REQ) }
    }

    override fun onUnlockRequest() {
        env.unlocked()
        forks.indices.forEach { forks[it] = ForkType.DIRTY }
        hungry = false
        inCS = false

        requested.indices
            .filter { requested[it] }
            .forEach {
                forks[it] = ForkType.ABSENT
                send(it, MsgType.REL)
                requested[it] = false
            }
    }
    private fun checkCSEnter(): Boolean {
        if (!hungry || forks.any { it == ForkType.ABSENT }) {
            return false
        }

        env.locked()
        inCS = true
        return true
    }

    private fun send(destId: Int, type: MsgType) {
        env.send(destId) {
            writeEnum(type)
        }
    }

    enum class MsgType { REQ, REL }
    enum class ForkType { CLEAN, DIRTY, ABSENT }
}
