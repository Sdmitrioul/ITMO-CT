package queue;

import java.util.Arrays;

public class ArrayQueueModule {
    private static int top = 0;
    private static int tail = 0;
    private static Object[] queue = new Object[10];

    //pre: element != null
    public static void enqueue(Object element) { //add new element in queue
        assert element != null;

        ensureCapacity(size() + 1);
        queue[tail] = element;
        tail = (tail + 1) % queue.length;
    }
    //post: size' == size + 1 && (∀i| 0 <= i < size && elements[cyclicIndex(head + i)]' == elements[cyclicIndex(head + i)]) && elements[tail]' == e && tail' = cyclicIndex(tail + 1)

    //pre: capacity > 0;
    private static void ensureCapacity(int capacity) {
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
    public static Object element() { //rerturn first element of queue
        assert size() != 0;
        assert queue[top] != null;

        return queue[top];
    }
    //post: immutable && R == elements[head]

    //pre: size != 0 && queue[top] != null;
    public static Object dequeue() { //return and delete first element
        assert size() != 0;
        assert queue[top] != null;

        Object tmp = queue[top];
        top = (top + 1) % queue.length;
        return tmp;
    }
    //post: size' == size - 1 && (∀i| 0 <= i < size' && elements[cyclicIndex(head + i)]' == elements[cyclicIndex(head + i)]) && head' == cyclicIndex(head + 1)

    //pre: true
    public static int size() {
        if (top > tail)
            return queue.length - top + tail;
        else
            return tail - top;
    }
    //post: immutable && R == size

    //pre: true
    public static boolean isEmpty() {
        return size() == 0;
    }
    //post: immutable && size == 0 ? R == true : R == false

    //pre: true
    public static void clear() {
        top = 0;
        tail = 0;
    }
    //post: head == 0 && tail == 0 && size == 0 && elements.length == INITIAL_CAPACITY

    //pre: true
    public static String toStr() {
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
