package linked_list_set;

import java.util.concurrent.atomic.AtomicMarkableReference;

public class SetImpl implements Set {
    private static class Node {
        AtomicMarkableReference<Node> next;
        int x;

        Node(int x, Node next) {
            this.next = new AtomicMarkableReference<>(next, false);
            this.x = x;
        }
    }

    private static class Window {
        final Node cur, next;
    
        public Window(Node cur, Node next) {
            this.cur = cur;
            this.next = next;
        }
    }

    private final Node head = new Node(Integer.MIN_VALUE, new Node(Integer.MAX_VALUE, null));

    /**
     * Returns the {@link Window}, where cur.x < x <= next.x
     */
    private Window findWindow(int x) {
        boolean[] removed = new boolean[1];
        
         retry: while (true) {
            Node cur = head;
            Node next = cur.next.getReference();
            
            while (next.x < x) {
                Node nextNext = next.next.get(removed);
                
                if (!removed[0]) {
                    cur = next;
                    next = cur.next.getReference();
                    continue;
                }
    
                if (!cur.next.compareAndSet(next, nextNext, false, false)) {
                    continue retry;
                }
                
                next = nextNext;
            }
            
            Node nextNext = next.next.get(removed);
            
            if (!removed[0]) {
                return new Window(cur, next);
            }
            
            cur.next.compareAndSet(next, nextNext, false, false);
        }
    }

    @Override
    public boolean add(int x) {
        while(true) {
            final Window w = findWindow(x);
            
            final Node cur = w.cur;
            final Node next = w.next;
            
            if (next.x == x) {
                return false;
            }
            
            final Node node = new Node(x, next);
            
            if (cur.next.compareAndSet(next, node, false, false)) {
                return true;
            }
        }
    }

    @Override
    public boolean remove(int x) {
        while(true) {
            final Window w = findWindow(x);
        
            final Node cur = w.cur;
            final Node next = w.next;
        
            if (next.x != x) {
                return false;
            }
        
            final Node node = next.next.getReference();
        
            if (next.next.compareAndSet(node, node, false, true)) {
                cur.next.compareAndSet(next, node, false, false);
                return true;
            }
        }
    }

    @Override
    public boolean contains(int x) {
        Window w = findWindow(x);
        return w.next.x == x;
    }
}