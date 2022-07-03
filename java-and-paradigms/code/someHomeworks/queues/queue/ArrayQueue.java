package queue;

//inv:
//elements.length >= 0
//0 <= head < elements.length;
//0 <= tail < elements.length;
//size < elements.length;
//size == head <= tail ? tail - head : elements.length - tail + head;
//head <= tail ? (∀i| head <= i < tail : elements[i] != null) :
// :(∀i| (tail < i || i >= head) : elements[i] != null);
public class ArrayQueue {
    private int top = 0;
    private int tail = 0;
    private Object[] queue = new Object[10];

    //pre: element != null
    public void enqueue(Object element) { //add new element in queue
        assert element != null;

        ensureCapacity(size() + 1);
        queue[tail] = element;
        tail = (tail + 1) % queue.length;
    }
    //post: size' == size + 1 && (∀i| 0 <= i < size && elements[cyclicIndex(head + i)]' == elements[cyclicIndex(head + i)]) && elements[tail]' == e && tail' = cyclicIndex(tail + 1)

    //pre: capacity > 0;
    private void ensureCapacity(int capacity) {
        //pre: len >= size
        if (capacity >= queue.length) {
            Object[] tmp = queue;
            queue = new Object[2 * tmp.length];
            if (tail < top) {
                System.arraycopy(tmp, top, queue, 0, tmp.length - top);
                System.arraycopy(tmp, 0, queue, tmp.length - top, tail);
            } else {
                System.arraycopy(tmp, 0, queue, 0, tail);
            }
            top = 0;
            tail = capacity - 1;
        }
        //post: immutable && ∀i| 0 <= i < size && R[i]' == elements[cyclicIndex(head + i)]
    }
    //post: capacity < elements.length ? immutable : (head' == 0 && tail' == size &&
    //&& ∀i| 0 <= i < size : elements[cyclicIndex(head + i)]' == elements[cyclicIndex(head + i)])

    //pre: size != 0 && queue[top] != null;
    public Object element() { //rerturn first element of queue
        assert size() != 0;
        assert queue[top] != null;

        return queue[top];
    }
    //post: immutable && R == elements[head]

    //pre: size != 0 && queue[top] != null;
    public Object dequeue() { //return and delete first element
        assert size() != 0;
        assert queue[top] != null;

        Object tmp = queue[top];
        top = (top + 1) % queue.length;
        return tmp;
    }
    //post: size' == size - 1 && (∀i| 0 <= i < size' && elements[cyclicIndex(head + i)]' == elements[cyclicIndex(head + i)]) && head' == cyclicIndex(head + 1)

    //pre: true
    public int size() {
        if (top > tail)
            return queue.length - top + tail;
        else
            return tail - top;
    }
    //post: immutable && R == size

    //pre: true
    public boolean isEmpty() {
        //return size() <= 0;
        return tail == top;
    }
    //post: immutable && size == 0 ? R == true : R == false

    //pre: true
    public void clear() {
        top = 0;
        tail = 0;
    }
    //post: head == 0 && tail == 0 && size == 0 && elements.length == INITIAL_CAPACITY

    //pre: true
    public String toStr() {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append('[');
        if (tail >= top) {
            for (int i = top; i < tail; i++) {
                stringBuilder.append(queue[i].toString()).append(", ");
            }
        } else {
            for (int i = top; i < queue.length; i++) {
                stringBuilder.append(queue[i].toString()).append(", ");
            }
            for (int i = 0; i < tail; i++) {
                stringBuilder.append(queue[i].toString()).append(", ");
            }
        }
        if (stringBuilder.length() != 1) {
            stringBuilder.deleteCharAt(stringBuilder.length() - 1);
            stringBuilder.deleteCharAt(stringBuilder.length() - 1);
        }
        stringBuilder.append(']');
        return stringBuilder.toString();
    }
    //post: immutable && R == "[" + elements[cyclicIndex(head)] + ", " + ... + ", " + elements[cyclicIndex(head + size - 1)] + "]"
}
