package queue;

public abstract class AbstractQueue implements Queue {
    //pre: element != null
    @Override
    public void enqueue(Object element) {
        assert element != null;

        put(element);
    }
    //post: size' == size + 1 && (∀i| 0 <= i < size && elements[cyclicIndex(head + i)]' == elements[cyclicIndex(head + i)]) && elements[tail]' == e && tail' = cyclicIndex(tail + 1)

    //pre: element != null
    protected abstract void put(Object element);
    //post: size' == size + 1 && (∀i| 0 <= i < size && elements[cyclicIndex(head + i)]' == elements[cyclicIndex(head + i)]) && elements[tail]' == e && tail' = cyclicIndex(tail + 1)

    //pre: size != 0
    @Override
    public Object element() {
        assert size() != 0;
        assert getFirst() != null;

        return getFirst();
    }
    //post: immutable && R == elements[head]

    //pre: size != 0
    protected abstract Object getFirst();
    //post: immutable && R == elements[head]

    //pre: size != 0
    @Override
    public Object dequeue() {
        assert size() != 0;
        assert getFirst() != null;

        Object tmp = getFirst();
        deleteFirst();
        return tmp;
    }
    //post: size' == size - 1 && (∀i| 0 <= i < size' && elements[cyclicIndex(head + i)]' == elements[cyclicIndex(head + i)]) && head' == cyclicIndex(head + 1)

    protected abstract void deleteFirst();

    //pre: true
    @Override
    public void clear() {
        delete();
    }
    //post: head == null && tail == null && size == 0 && elements.length == INITIAL_CAPACITY

    //pre: true
    protected abstract void delete();
    //post: head == null && tail == null && size == 0 && elements.length == INITIAL_CAPACITY

    //pre: true
    @Override
    public int size() {
        return thisSize();
    }
    //post: immutable && R == size

    //pre: true
    protected abstract int thisSize();
    //post: immutable && R == size

    //pre: true
    @Override
    public Object[] toArray() {
        return array();
    }
    //post: immutable && R == array (∀i| array[i] = queue[i])

    //pre: true
    protected abstract Object[] array();
    //post: immutable && R == array (∀i| array[i] = queue[i])

    //pre: true
    @Override
    public boolean isEmpty() {
        return empty();
    }
    //post: immutable && (size == 0 ? R == true : R == false || tail == null && head == null ? R == true : R == false)

    //pre: true
    protected abstract boolean empty();
    //post: immutable && (size == 0 ? R == true : R == false || tail == null && head == null ? R == true : R == false)
}
