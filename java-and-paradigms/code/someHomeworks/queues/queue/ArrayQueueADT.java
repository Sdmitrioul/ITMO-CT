package queue;

import java.util.Arrays;

public class ArrayQueueADT {
    private /*static*/ int top = 0;
    private /*static*/ int tail = 0;
    private /*static*/ Object[] queue = new Object[10];

    //pre: element != null && queueADT != null
    public static void enqueue(ArrayQueueADT queueADT, Object element) { //add new element in queue
        assert element != null;
        assert queueADT != null;

        ensureCapacity(queueADT, queueADT.size(queueADT) + 1);
        queueADT.queue[queueADT.tail] = element;
        queueADT.tail = (queueADT.tail + 1) % queueADT.queue.length;
    }
    //post: size' == size + 1 && (∀i| 0 <= i < size && elements[cyclicIndex(head + i)]' == elements[cyclicIndex(head + i)]) && elements[tail]' == e && tail' = cyclicIndex(tail + 1)

    //pre: capacity > 0 && queueADT != null
    private static void ensureCapacity(ArrayQueueADT queueADT, int capacity) {
        assert queueADT != null;
        //pre: len >= size
        if (capacity >= queueADT.queue.length) {
            Object[] tmp = queueADT.queue;
            queueADT.queue = new Object[2 * tmp.length];
            if (queueADT.tail < queueADT.top) {
                System.arraycopy(tmp, queueADT.top, queueADT.queue, 0, tmp.length - queueADT.top);
                System.arraycopy(tmp, 0, queueADT.queue, tmp.length - queueADT.top, queueADT.tail);
            } else {
                System.arraycopy(tmp, 0, queueADT.queue, 0, queueADT.tail);
            }
            queueADT.top = 0;
            queueADT.tail = capacity - 1;
        }
        //post: immutable && ∀i| 0 <= i < size && R[i]' == elements[cyclicIndex(head + i)]
    }
    //post: capacity < elements.length ? immutable : (head' == 0 && tail' == size &&
    //&& ∀i| 0 <= i < size : elements[cyclicIndex(head + i)]' == elements[cyclicIndex(head + i)])

    //pre: size != 0 && queue[top] != null && queueADT != null
    public static Object element(ArrayQueueADT queueADT) { //rerturn first element of queue
        assert queueADT.size(queueADT) != 0;
        assert queueADT.queue[queueADT.top] != null;
        assert queueADT != null;

        return queueADT.queue[queueADT.top];
    }
    //post: immutable && R == elements[head]

    //pre: size != 0 && queue[top] != null && queueADT != null
    public static Object dequeue(ArrayQueueADT queueADT) { //return and delete first element
        assert queueADT.size(queueADT) != 0;
        assert queueADT.queue[queueADT.top] != null;
        assert queueADT != null;

        Object tmp = queueADT.queue[queueADT.top];
        queueADT.top = (queueADT.top + 1) % queueADT.queue.length;
        return tmp;
    }
    //post: size' == size - 1 && (∀i| 0 <= i < size' && elements[cyclicIndex(head + i)]' == elements[cyclicIndex(head + i)]) && head' == cyclicIndex(head + 1)

    //pre: queueADT != null
    public static int size(ArrayQueueADT queueADT) {
        assert queueADT != null;

        if (queueADT.top > queueADT.tail)
            return queueADT.queue.length - queueADT.top + queueADT.tail;
        else
            return queueADT.tail - queueADT.top;
    }
    //post: immutable && R == size

    //pre: queueADT != null
    public static boolean isEmpty(ArrayQueueADT queueADT) {
        assert queueADT != null;

        return size(queueADT) <= 0;
    }
    //post: immutable && size == 0 ? R == true : R == false

    //pre: queueADT != null
    public static void clear(ArrayQueueADT queueADT) {
        assert queueADT != null;

        queueADT.top = 0;
        queueADT.tail = 0;
    }
    //post: head == 0 && tail == 0 && size == 0 && elements.length == INITIAL_CAPACITY

    //pre: queueADT != null
    public static String toStr(ArrayQueueADT queueADT) {
        assert queueADT != null;

        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append('[');
        if (queueADT.tail >= queueADT.top) {
            for (int i = queueADT.top; i < queueADT.tail; i++) {
                stringBuilder.append(queueADT.queue[i].toString()).append(", ");
            }
        } else {
            for (int i = queueADT.top; i < queueADT.queue.length; i++) {
                stringBuilder.append(queueADT.queue[i].toString()).append(", ");
            }
            for (int i = 0; i < queueADT.tail; i++) {
                stringBuilder.append(queueADT.queue[i].toString()).append(", ");
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
