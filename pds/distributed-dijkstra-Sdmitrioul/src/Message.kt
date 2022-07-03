package dijkstra.messages

sealed class Message

data class DijkstraMessage(val type: MessageType, val info: Long? = null): Message()

enum class MessageType {
    OK, REFUSE, INFO, FINISH
}
