package queue;

public interface Queue {
    //pre: element != null
    void enqueue(Object element);
    //post: size' == size + 1 && (∀i| 0 <= i < size && elements[cyclicIndex(head + i)]' == elements[cyclicIndex(head + i)]) && elements[tail]' == e && tail' = cyclicIndex(tail + 1)

    //pre: true
    void clear();
    //post: head == null && tail == null && size == 0 && elements.length == INITIAL_CAPACITY

    //pre: size != 0;
    Object element();
    //post: immutable && R == elements[head]

    //pre: size != 0
    Object dequeue();
    //post: size' == size - 1 && (∀i| 0 <= i < size' && elements[cyclicIndex(head + i)]' == elements[cyclicIndex(head + i)]) && head' == cyclicIndex(head + 1)

    //pre: true
    int size();
    //post: immutable && R == size

    //pre: true
    boolean isEmpty();
    //post: immutable && (size == 0 ? R == true : R == false || tail == null && head == null ? R == true : R == false)

    //pre: true
    Object[] toArray();
    //post: immutable && R == array (∀i| array[i] = queue.element -> i)
}
