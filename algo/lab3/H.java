import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class H {
    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);

        int size = 0;

        if (scanner.hasLine()) {
            size = scanner.getNextInt();
        }

        String[] strings = new String[size];

        for (int i = 0; i < size; i++) {
            if (scanner.hasLine()) {
                strings[i] = scanner.getLine();
            }
        }

        String example = "";

        if (scanner.hasLine()) {
            example = scanner.getLine();
        }

        Aho auto = new Aho(strings);

        BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out));

        int[] result = auto.contains(example);

        StringBuilder stringBuilder = new StringBuilder();
        for (int ans : result) {
            stringBuilder.append(ans).append("\n");
        }

        writer.write(stringBuilder.toString());
        writer.flush();
    }

    private static class Aho {
        private Node root;
        private ArrayList<Node> nodes = new ArrayList<>();
        private int position = -1;

        public Aho(String[] strings) {
            root = new Node(null, -1, 0);
            root.getted = true;
            nodes.add(root);
            for (String string : strings) {
                addString(string);
            }
        }

        private void addString(String string) {
            Node cur = root;
            for (int i = 0; i < string.length(); i++) {
                int c = string.charAt(i) - 'a';
                if (cur.sons[c] == null) {
                    nodes.add(new Node(cur, c, nodes.size()));
                    cur.sons[c] = nodes.get(nodes.size() - 1);
                }
                cur = cur.sons[c];
            }
            cur.leafs.add(++position);
        }

        private Node getSuffLink(Node node) {
            if (node.suffLink == null) {
                if (node == root || node.parent == root) {
                    node.suffLink = root;
                } else {
                    node.suffLink = getLink(getSuffLink(node.parent), node.symbol);
                }
            }
            return node.suffLink;
        }

        private Node getLink(Node node, int c) {
            if (node.go[c] == null) {
                if (node.sons[c] != null) {
                    node.go[c] = node.sons[c];
                } else if (node == root) {
                    node.go[c] = root;
                } else {
                    node.go[c] = getLink(getSuffLink(node), c);
                }
            }
            return node.go[c];
        }

        private Node getUp(Node node) {
            if (node.up == null) {
                if (getSuffLink(node).isLeaf()) {
                    node.up = getSuffLink(node);
                } else if (getSuffLink(node) == root) {
                    node.up = root;
                } else {
                    node.up = getUp(getSuffLink(node));

                }
            }
            return node.up;
        }

        private Set<Integer> getAllLeafs(Node node) {
            if (!node.getted) {
                node.leafs.addAll(getAllLeafs(getUp(node)));
                node.getted = true;
            }
            return node.leafs;
        }

        private int[] contains(String string) {
            HashSet<Integer> visited = new HashSet<>();
            Node cur = root;
            for (int i = 0; i < string.length(); i++) {
                int c = string.charAt(i) - 'a';
                cur.count++;
                visited.add(cur.position);
                cur = getLink(cur, c);
            }
            cur.count++;
            visited.add(cur.position);
            return counter(visited);
        }

        private int[] counter(Set<Integer> visited) {
            int[] counter = new int[position + 1];
            for (int i : visited) {
                Node cur = nodes.get(i);
                int count = cur.count;
                for (int j : getAllLeafs(cur)) {
                    counter[j] += count;
                }
            }
            return counter;
        }

        private class Node {
            Node[] go = new Node[26];
            Node[] sons = new Node[26];
            final Node parent;
            final int symbol;
            final int position;
            boolean visited = false;
            boolean getted = false;
            Node up;
            Node suffLink;
            HashSet<Integer> leafs = new HashSet<>();
            int count = 0;

            public Node(Node parent, int symbol, int position) {
                this.parent = parent;
                this.symbol = symbol;
                this.position = position;
            }

            public boolean isLeaf() {
                return !leafs.isEmpty();
            }

            public boolean isRoot() {
                return position == 0;
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


