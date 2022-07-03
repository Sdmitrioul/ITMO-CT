package queue;

public class ArrayQueue extends AbstractQueue {
    private int head = 0;
    private int tail = 0;
    private Object[] queue = new Object[10];

    //pre: capacity > 0;
    private void ensureCapacity(int capacity) {
        //pre: len >= size
        if (capacity >= queue.length) {
            Object[] tmp = queue;
            queue = new Object[2 * tmp.length];
            if (tail < head) {
                System.arraycopy(tmp, head, queue, 0, tmp.length - head);
                System.arraycopy(tmp, 0, queue, tmp.length - head, tail);
            } else {
                System.arraycopy(tmp, 0, queue, 0, tail);
            }
            head = 0;
            tail = capacity - 1;
        }
        //post: immutable && ∀i| 0 <= i < size && R[i]' == elements[cyclicIndex(head + i)]
    }
    //post: capacity < elements.length ? immutable : (head' == 0 && tail' == size &&
    //&& ∀i| 0 <= i < size : elements[cyclicIndex(head + i)]' == elements[cyclicIndex(head + i)])

    @Override
    protected void put(Object element) {
        ensureCapacity(size() + 1);
        queue[tail] = element;
        tail = (tail + 1) % queue.length;
    }

    @Override
    protected void deleteFirst() {
        head = (head + 1) % queue.length;
    }

    @Override
    protected Object getFirst() {
        return queue[head];
    }

    @Override
    protected void delete() {
        head = 0;
        tail = 0;
        queue = new Object[10];
    }

    @Override
    protected int thisSize() {
        if (head > tail)
            return queue.length - head + tail;
        else
            return tail - head;
    }

    @Override
    protected Object[] array() {
        Object[] tmp = new Object[size()];
        if (tail < head) {
            System.arraycopy(queue, head, tmp, 0, queue.length - head);
            System.arraycopy(queue, 0, tmp, queue.length - head, tail);
        } else {
            System.arraycopy(queue, head, tmp, 0, size());
        }
        return tmp;
    }

    @Override
    protected boolean empty() {
        return thisSize() == 0;
    }
}
