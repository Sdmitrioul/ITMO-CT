import system.DataHolderEnvironment

class DataHolderImpl<T : Comparable<T>>(
    private val keys: List<T>,
    private val dataHolderEnvironment: DataHolderEnvironment
) : DataHolder<T> {
    private var checkedIndex = 0
    private var rawIndex = 0

    override fun checkpoint() {
        checkedIndex = rawIndex
    }

    override fun rollBack() {
        rawIndex = checkedIndex
    }

    override fun getBatch(): List<T> {
        val prev = rawIndex

        rawIndex = (rawIndex + dataHolderEnvironment.batchSize).coerceAtMost(keys.size)

        return keys.subList(prev, rawIndex)
    }
}