package queue;

public class LinkedQueue extends AbstractQueue {
    private Node head;
    private Node tail = head;
    //private int size;

    @Override
    protected void put(Object element) {
        if (tail != null) {
            Node node = new Node(element);
            tail.setNext(node);
            tail = node;
        } else {
            tail = new Node(element);
            head = tail;
        }
        //size++;
    }

    @Override
    protected void deleteFirst() {
        if (head == tail) {
            tail = null;
            head = tail;
        } else {
            head = head.getNext();
        }
        //size--;
    }

    @Override
    protected Object getFirst() {
        return head.getValue();
    }

    @Override
    protected void delete() {
        tail = null;
        head = null;
        //size = 0;
    }

    @Override
    protected int thisSize() {
        if (head == null) {
            return 0;
        }
        Node tmp = head;
        int size = 1;
        while (tmp.getNext() != null) {
            size++;
            tmp = tmp.getNext();
        }
        return size;
    }

    //from head to tail; tmp = tmp.next (if tmp.next != null)
    @Override
    protected Object[] array() {
        int size = size();
        Object[] massive = new Object[size];
        Node tmp = head;
        for (int i = 0; i < size; i++) {
            massive[i] = tmp.getValue();
            tmp = tmp.getNext();
        }
        return massive;
    }

    @Override
    protected boolean empty() {
        return tail == null && head == null;
    }

    private class Node {
        private Object value;
        private Node next; //prev << this << next

        public Node(Object value) {
            assert value != null;

            this.value = value;
        }

        public void setNext(Node next) {
            this.next = next;
        }

        public Node getNext() {
            return next;
        }

        public Object getValue() {
            return value;
        }
    }
}
