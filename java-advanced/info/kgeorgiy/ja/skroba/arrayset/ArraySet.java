package info.kgeorgiy.ja.skroba.arrayset;

import java.util.*;

public class ArraySet<E> extends AbstractSet<E> implements SortedSet<E> {
    private final  String OPERATION_EXCEPTION = " - operation doesn't supported by ArraySet";
    private final List<E> set;
    private final Comparator<? super E> comparator;


    public ArraySet(Collection<? extends E> collection, Comparator<? super E> comparator) {
        this.comparator = comparator;
        this.set = preProcess(collection, comparator);
    }

    public ArraySet(Collection<? extends E> collection) {
        this(collection, null);
    }

    public ArraySet(Comparator<? super E> comparator) {
       this(Collections.emptyList(), comparator);
    }

    public ArraySet() {
        this(Collections.emptyList(), null);
    }

    private List<E> preProcess(Collection<? extends E> collection, Comparator<? super E> comparator) {
        TreeSet<E> set = new TreeSet<>(comparator);
        set.addAll(collection);
        return Collections.unmodifiableList(List.copyOf(set));
    }

    @Override
    public Comparator<? super E> comparator() {
        return comparator;
    }

    private int search(E e) {
        Objects.requireNonNull(e);
        return Collections.binarySearch(set, e, comparator);
    }

    private SortedSet<E> subSet(E fromElement, boolean inFrom, E toElement, boolean inTo) {
        int from = search(fromElement);
        int to = search(toElement);
        from = from < 0 && inFrom ? -from - 1 : from < 0 ? -from : from;
        to = to < 0 && inTo ? -to : to < 0 ? - to - 1 : inTo ? to + 1 : to;
        return to <= from ? new ArraySet<>(comparator) : new ArraySet<>(set.subList(from, to), comparator);
    }

    @Override
    public SortedSet<E> subSet(E fromElement, E toElement) {
        if (comparator.compare(fromElement, toElement) > 0) {
            throw new IllegalArgumentException(String.format("%s is higher than %s", fromElement.toString(), toElement.toString()));
        }
        return subSet(fromElement, true, toElement, false);
    }

    @Override
    public SortedSet<E> headSet(E toElement) {
        if (isEmpty()) {
            return this;
        }
        return subSet(first(), true, toElement, false);
    }

    @Override
    public SortedSet<E> tailSet(E fromElement) {
        if (isEmpty()) {
            return new ArraySet<>(comparator);
        }
        return subSet(fromElement, true, last(), true);
    }

    @Override
    public E first() {
        if (!set.isEmpty()) {
            return set.get(0);
        } else {
            throw new NoSuchElementException("Set is empty");
        }
    }

    @Override
    public E last() {
        if (!set.isEmpty()) {
            return set.get(size() - 1);
        } else {
            throw new NoSuchElementException("Set is empty");
        }
    }

    @Override
    public int size() {
        return set.size();
    }

    @Override
    public boolean isEmpty() {
        return set.isEmpty();
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean contains(Object o) {
        return search((E) o) >= 0;
    }

    @Override
    public Iterator<E> iterator() {
        return set.iterator();
    }
}
