package msqueue;

import kotlinx.atomicfu.AtomicRef;

public class MSQueue implements Queue {
    private final AtomicRef<Node> head;
    private final AtomicRef<Node> tail;

    public MSQueue() {
        final Node dummy = new Node(0);
        this.head = new AtomicRef<>(dummy);
        this.tail = new AtomicRef<>(dummy);
    }

    @Override
    public void enqueue(int x) {
        final Node newTail = new Node(x);
        
        while (true) {
            final Node tail = this.tail.getValue();
            
            if (tail.next.compareAndSet(null, newTail)) {
                this.tail.compareAndSet(tail, newTail);
                return;
            }
    
            this.tail.compareAndSet(tail, tail.next.getValue());
        }
    }

    @Override
    public int dequeue() {
        while (true) {
            final Node head = this.head.getValue();
            final Node headNext = head.next.getValue();
            
            if (headNext == null) {
                return Integer.MIN_VALUE;
            }
    
            this.tail.compareAndSet(head, headNext);
            
            if (this.head.compareAndSet(head, headNext)) {
                return headNext.x;
            }
        }
    }

    @Override
    public int peek() {
        final Node curHead = head.getValue().next.getValue();
        
        if (curHead == null) {
            return Integer.MIN_VALUE;
        }
        
        return curHead.x;
    }

    private class Node {
        final int x;
        AtomicRef<Node> next = new AtomicRef<>(null);

        Node(int x) {
            this.x = x;
        }
    }
}