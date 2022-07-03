package queue;

import java.util.Queue;

public class ArrayQueueTest {
    private static void fill(ArrayQueue queue, ArrayQueueADT queueADT, ArrayQueueModule queueModule) {
        for (int i = 0; i < 10; i++) {
            queue.enqueue(i);
            queueADT.enqueue(queueADT, i);
            queueModule.enqueue(i);
        }
    }

    private static void dump(ArrayQueue queue, ArrayQueueADT queueADT, ArrayQueueModule queueModule) {
        while (!queue.isEmpty()) {
            System.out.println(queue.size() + " " +
                    queue.element() + " " + queue.dequeue());
        }
        while (!queueADT.isEmpty(queueADT)) {
            System.out.println(queueADT.size(queueADT) + " " + queueADT.element(queueADT) + " " + queueADT.dequeue(queueADT));
        }
        while (!queueModule.isEmpty()){
            System.out.println(queueModule.size() + " " + queueModule.element() + " " + queueModule.dequeue());
        }
    }

    public static void main(String[] args) {
        ArrayQueue queue = new ArrayQueue();
        ArrayQueueADT queueADT = new ArrayQueueADT();
        ArrayQueueModule queueModule = new ArrayQueueModule();
        fill(queue, queueADT, queueModule);
        dump(queue, queueADT, queueModule);
    }
}
