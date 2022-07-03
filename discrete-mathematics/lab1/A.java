import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class A {
    private static int size;
    private static boolean[][] matrix;

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);
        if (scanner.hasLine()) {
            size = scanner.getNextInt();
        }

        matrix = new boolean[size][size];

        for (int i = 0; i < size; i++) {
            if (scanner.hasLine()) {
                String line = scanner.getLine();
                for (int j = 0; j < i; j++) {
                    matrix[i][j] = matrix[j][i] = '1' == line.charAt(j);
                }
            }
        }

        MyQueue answer = findHamiltonCycle();

        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out));
        writer.write(answer.toString());
        writer.flush();
    }

    private static MyQueue findHamiltonCycle() {
        MyQueue queue = new MyQueue();

        for (int i = 0; i < size; i++) {
            queue.add(i);
        }

        for (int i = 0; i < size * (size - 1); i++) {
            if (!matrix[queue.get(0)][queue.get(1)]) {
                int j = 2;
                while (!matrix[queue.get(0)][queue.get(j)] || (/*j + 1 < queue.size() &&*/ !matrix[queue.get(1)][queue.get(j + 1)])) {
                    j++;
                }
                queue.reverse(1, j);
            }
            queue.add(queue.removeFirst());
        }

        return queue;
    }

    private static class MyQueue {
        private Node first;
        private Node last;
        private int size = 0;
        boolean empty = true;

        public int get(int j) {
            Node cur = first;
            for (int i = 0; i < j; i++) {
                cur = cur.next;
            }
            return cur.getNode();
        }

        private Node getNode(int j) {
            Node cur = first;
            for (int i = 0; i < j; i++) {
                cur = cur.next;
            }
            return cur;
        }

        public void reverse(int from, int to) {
            Node left = getNode(from);
            Node right = getNode(to);
            for (int i = 0; i < (to - from + 1) / 2; i++) {
                int tmp = left.getNode();
                left.setNode(right.getNode());
                right.setNode(tmp);
                left = left.next;
                right = right.prev;
            }
        }

        public int getSize() {
            return size;
        }

        public void add(int i) {
            if (!this.empty) {
                last.setNext(new Node(last, i));
                last = last.next;
            } else {
                first = new Node(null, i);
                last = first;
                empty = false;
            }
            size++;
        }

        public int removeFirst() {
            size--;
            int ans = first.getNode();
            first = first.next;
            first.prev = null;
            return ans;
        }

        @Override
        public String toString() {
            StringBuilder stringBuilder = new StringBuilder();
            Node cur = first;
            while (cur.next != null) {
                stringBuilder.append(cur.getNode() + 1).append(" ");
                cur = cur.next;
            }
            stringBuilder.append(cur.getNode() + 1);
            return stringBuilder.toString();
        }

        class Node {
            private Node prev;
            private Node next;
            private int node;

            public Node(Node prev, int node) {
                this.prev = prev;
                this.node = node;
            }

            public Node getPrev() {
                return prev;
            }

            public void setPrev(Node prev) {
                this.prev = prev;
            }

            public Node getNext() {
                return next;
            }

            public void setNext(Node next) {
                this.next = next;
            }

            public int getNode() {
                return node;
            }

            public void setNode(int node) {
                this.node = node;
            }
        }
    }

    private static class Scanner {
        private BufferedReader reader;
        private String line;
        private boolean readed;
        private int index = 0;
        private int position = 0;
        private String mark;

        public Scanner(InputStream in) throws UnsupportedEncodingException {
            try{
                reader = new BufferedReader(new InputStreamReader(in, StandardCharsets.UTF_8.name()));
            } catch (UnsupportedEncodingException e){
                throw new UnsupportedEncodingException("Encode error :" + e.getMessage());
            }
        }

        public Scanner(String s) throws FileNotFoundException, UnsupportedEncodingException {
            try{
                reader = new BufferedReader(new InputStreamReader(new FileInputStream(s), StandardCharsets.UTF_8.name()));
            } catch (FileNotFoundException e){
                throw new FileNotFoundException("File not found :" + e.getMessage());
            } catch (UnsupportedEncodingException e){
                throw new UnsupportedEncodingException("Encode error :" + e.getMessage());
            }
        }

        public boolean hasNextInt() {
            while (index < line.length() && Character.isWhitespace(line.charAt(index))){
                index++;
            }
            return index != line.length();
        }

        public boolean hasNextWord() {
            while (index < line.length() && !isWordSymbol(line.charAt(index))){
                //System.out.println(line.charAt(index) + " " + Character.isLetter(line.charAt(index)));
                index++;
            }
            return index != line.length();
        }

        private boolean isWordSymbol(char c) {
            return Character.isLetter(c) || Character.getType(c) == Character.DASH_PUNCTUATION ||  c == '\'';
        }

        public int getNextInt() {
            if (!hasNextInt()) {
                throw new NoSuchElementException();
            }
            int begin = index;
            while (!Character.isWhitespace(line.charAt(index))){
                index++;
                if (index == line.length()){
                    break;
                }
            }
            return Integer.parseInt(line.substring(begin, index));
        }

        public String getNextWord() {
            if (!hasNextWord()) {
                throw new NoSuchElementException();
            }
            int begin = index;
            while (isWordSymbol(line.charAt(index))){
                index++;
                if (index == line.length()){
                    break;
                }
            }
            return line.substring(begin, index);
        }

        public boolean hasLine() throws IOException {
            try{
                index = 0;
                readed = true;
                line = reader.readLine();
            } catch (IOException e) {
                throw new IOException("Input error in Scanner.hasLine(): " + e.getMessage());
            }
            return line != null;
        }

        public String getLine() throws IOException {
            if (readed) {
                readed = false;
                return line;
            } else {
                if (hasLine()) {
                    readed = false;
                    return line;
                } else {
                    return "";
                }
            }
        }

        public void close() throws IOException {
            try{
                reader.close();
            } catch(IOException e) {
                throw new IOException("Eror: has problem in scanner.close() " + e.getMessage());
            }
        }
    }
}
