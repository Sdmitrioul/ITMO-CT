class ConsistentHashImpl<K> : ConsistentHash<K> {
    private val nodes = ArrayList<Pair<Shard, Int>>()

    override fun getShardByKey(key: K): Shard {
        if (nodes.isEmpty()) {
            throw NoSuchElementException()
        }

        val hash = key.hashCode()

        if (hash > nodes.last().second) {
            return nodes.first().first
        }

        var left = 0
        var right = nodes.size - 1
        var middle = mid(left, right)

        while (left + 1 < right) {
            val middleHash = nodes[middle].second

            if (hash <= middleHash) {
                right = middle
            } else {
                left = middle
            }

            middle = mid(left, right)
        }

        return if (hash <= nodes[left].second) nodes[left].first else nodes[right].first
    }

    override fun addShard(newShard: Shard, vnodeHashes: Set<Int>): Map<Shard, Set<HashRange>> {
        vnodeHashes.map { newShard to it }.forEach { nodes.add(it) }

        nodes.sortBy { it.second }

        return showChanges(newShard)
    }

    override fun removeShard(shard: Shard): Map<Shard, Set<HashRange>> {
        val changes = showChanges(shard)

        nodes.removeIf { shard == it.first }

        return changes
    }

    private fun showChanges(shard: Shard): Map<Shard, Set<HashRange>> {
        val index = nodes.indexOfFirst { it.first != shard }

        if (index == -1) {
            return emptyMap()
        }

        val map = HashMap<Shard, HashSet<HashRange>>()

        val from = index + 1
        val to = nodes.size + from

        val element = nodes[index]

        var anotherHash = element.second

        for (ind in from .. to) {
            val (currentShard, currentHash) = nodes[indexOf(ind)]
            val (nextShard, _) = nodes[indexOf(ind + 1)]

            if (currentShard != shard) {
                anotherHash = currentHash

                continue
            }

            if (nextShard == shard) continue

            val range = HashRange(anotherHash + 1, currentHash)

            map.merge(nextShard, hashSetOf(range)) {
                prev, cur -> prev.addAll(cur)
                prev
            }
        }

        return map
    }

    private fun indexOf(value: Int): Int = value % nodes.size

    private fun mid(left: Int, right: Int): Int = right - (right - left) / 2
}