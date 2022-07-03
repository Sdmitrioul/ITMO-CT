package stack;

import kotlinx.atomicfu.AtomicArray;
import kotlinx.atomicfu.AtomicRef;

import java.util.Random;

import static java.lang.Math.max;
import static java.lang.Math.min;

public class StackImpl implements Stack {
    private final static int BUFFER_SIZE = 32;
    private final static int WAITING_TIME = 100;
    private final static int COUNT_OF_TRIES = 3;
    private final static int NEAR_BOUND = 2;
    
    private final static Random random = new Random();
    
    private static class Node {
        final AtomicRef<Node> next;
        final int x;

        Node(int x, Node next) {
            this.next = new AtomicRef<>(next);
            this.x = x;
        }
    }

    // head pointer
    private final AtomicRef<Node> head = new AtomicRef<>(null);
    private final AtomicArray<Integer> buffer = new AtomicArray<>(BUFFER_SIZE);

    @Override
    public void push(int x) {
        if (eliminatePush(x)) {
            return;
        }
        
        while (true) {
            Node h = head.getValue();
            Node newHead = new Node(x, h);
            if (head.compareAndSet(h, newHead)) {
                return;
            }
        }
    }
    
    private boolean eliminatePush(final Integer x) {
        for (int j = 0; j < COUNT_OF_TRIES; j++) {
            final int randomStart = random.nextInt(BUFFER_SIZE);
        
            for (int i = max(0, randomStart - NEAR_BOUND); i < min(BUFFER_SIZE, randomStart + NEAR_BOUND); i++) {
                if (!buffer.get(getPos(i)).compareAndSet(null, x)) {
                    continue;
                }
                
                return tryEliminate(i, x);
            }
        }
    
        return false;
    }
    
    private boolean tryEliminate(final int index, final Integer x) {
        for (int i = 0; i < WAITING_TIME; i++) {
            Integer val = buffer.get(getPos(index)).getValue();
            
            if (val == null || val != x) {
                return true;
            }
        }
        
        return !buffer.get(getPos(index)).compareAndSet(x, null);
    }

    @Override
    public int pop() {
        final Integer value = tryInBuffer();
        
        if (value != null) {
            return value;
        }
        
        while (true) {
            Node h = head.getValue();
            
            if (h == null) {
                return Integer.MIN_VALUE;
            }
            
            if (head.compareAndSet(h, h.next.getValue())) {
                return h.x;
            }
        }
    }
    
    private Integer tryInBuffer() {
        for (int i = 0; i < BUFFER_SIZE * COUNT_OF_TRIES; i++) {
            Integer value = buffer.get(getPos(i)).getValue();
            
            if (value != null && buffer.get(getPos(i)).compareAndSet(value, null)) {
                return value;
            }
        }
        
        return null;
    }
    
    private static int getPos(int pos) {
        return pos % BUFFER_SIZE;
    }
}
