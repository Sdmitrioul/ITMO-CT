import system.MergerEnvironment

class MergerImpl<T : Comparable<T>>(
    private val mergerEnvironment: MergerEnvironment<T>,
    prevStepBatches: Map<Int, List<T>>?
) : Merger<T> {

    private val batches : HashMap<Int, List<T>> = hashMapOf()

    init {
        if (prevStepBatches != null) {
            prevStepBatches.forEach {
                batches[it.key] = it.value
            }
        } else {
            (0 until mergerEnvironment.dataHoldersCount).forEach {
                batches[it] = mergerEnvironment.requestBatch(it)
            }
        }
    }

    override fun mergeStep(): T? {
        val min = batches.toList().minWithOrNull { f, s -> f.second.first().compareTo(s.second.first()) } ?: return null

        val result = min.second.first()

        val list = min.second.let { it.subList(1, it.size) }
        val position = min.first

        if (list.isNotEmpty()) {
            batches[position] = list

            return result
        }

        val reqBatch = mergerEnvironment.requestBatch(position)

        if (reqBatch.isEmpty()) {
            batches.remove(position)
        } else {
            batches[position] = reqBatch
        }

        return result
    }

    override fun getRemainingBatches(): Map<Int, List<T>> {
        return batches
    }
}