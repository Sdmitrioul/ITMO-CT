/*import java.util.*;

public class StringTree {
    private Node root = new Node(-1, '#', 0);
    private ArrayList<Node> nodes = new ArrayList<>();
    private int counter = -1;

    public StringTree(String[] strings) {
        makeTree(strings);
    }

    public Set<Integer> contains(String example) {
        HashSet<Integer> set = new HashSet<>();
        Set<Integer> visited = new HashSet<>();
        Node cur = root;
        for (char c : example.toCharArray()) {
            visited.add(cur.getPosition());
            if (cur.hasChild(c)) {
                cur = node(cur.getChild(c));
            } else {
                cur = node(getLink(cur.getPosition(), c));
            }
        }
        visited.add(cur.getPosition());
        return result(visited);
    }

    private Set<Integer> result(Set<Integer> visited) {
        HashSet<Integer> result = new HashSet<>();
        for (int i : visited) {
            Node cur = node(i);
            if (cur.isTerminal()) {
                result.add(cur.getTerminal());
            }
            addTerminals(result, cur);
            cur = node(cur.getCompactHref());
            while (!cur.isRoot()) {
                addTerminals(result, cur);
                Node tmp = node(cur.getCompactHref());
                cur.setCompactHref(0);
                cur = tmp;
            }
        }
        return result;
    }

    private void addTerminals(HashSet<Integer> result, Node cur) {
        if (cur.isTerminal()) {
            result.add(cur.getTerminal());
        }
        while (cur.getPrevTerminal() != 0 && node(cur.getPrevTerminal()).getTerminal() != -1) {
            cur = node(cur.getPrevTerminal());
            result.add(cur.getTerminal());
        }
    }


    //Making Tree
    //-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    private void makeTree(String[] strings) {
        nodes.add(root);
        for (String string : strings) {
            putString(string);
        }
        dfs(root);
        bfs();
    }

    private void bfs() {
        Queue<Integer> queue = new ArrayDeque<>();
        queue.add(0);
        while (!queue.isEmpty()) {
            Node current = node(queue.remove());
            int[] children = current.getAllChildren();

            for (int i : children) {
                Node child = node(i);
                if (current.isRoot()) {
                    child.setHref(0);
                    child.setCompactHref(0);
                } else {
                    child.setHref(getLink(child.getParent(), child.getEdge()));

                    if (node(child.getHref()).isTerminal() || node(child.getHref()).isRoot()) {
                        child.setCompactHref(child.getHref());
                    } else {
                        child.setCompactHref(node(child.getHref()).getCompactHref());
                    }
                }
                queue.add(child.getPosition());
            }
        }
    }

    private int getLink(int parent, char edge) {
        if (node(node(parent).getHref()).hasChild(edge)) {
            return node(node(parent).getHref()).getChild(edge);
        } else if (node(parent).getHref() == node(node(parent).getHref()).getHref()) {
            return node(parent).getHref();
        } else {
            return getLink(node(parent).getHref(), edge);
        }
    }

    private void dfs(Node node) {
        int[] children = node.getAllChildren();
        int terminal = node.isTerminal() ? node.getPosition() : node.getPrevTerminal();
        for (int i : children) {
            Node child = node(i);
            child.setPrevTerminal(terminal);
            dfs(child);
        }
    }

    private void putString(String string) {
        Node cur = root;
        for (char c : string.toCharArray()) {
            cur = addChild(cur, c);
        }
        cur.setTerminal(++counter);
    }

    //End making tree
    //-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------


    //Helper functions
    //-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    private Node node(int i) {
        return i > -1 && i < nodes.size() ? nodes.get(i) : null;
    }

    private Node addChild(Node node, char c) {
        Node child = node(node.getChild(c));
        if (child == null) {
            child = new Node(node.getPosition(), c, nodes.size());
            node.addChild(child);
            nodes.add(child);
        }
        return child;
    }

    // End of Helper functions
    //-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

    private class Node {
        private final int parent;
        private final char edge;
        private final int position;
        private int terminal; //word
        private int href = 0;
        private int compactHref = 0;
        private int prevTerminal = 0; //position in list
        private HashMap<Character, Integer> children = new HashMap<>();

        public Node(int parent, char edge, int position) {
            this.parent = parent;
            this.edge = edge;
            this.position = position;
            this.terminal = -1;
        }

        public Node(int parent, char edge, int position, int terminal) {
            this.parent = parent;
            this.edge = edge;
            this.position = position;
            this.terminal = terminal;
        }

        public boolean isRoot() {
            return parent == -1;
        }

        public boolean isTerminal() {
            return terminal != -1;
        }

        public int getParent() {
            return parent;
        }

        public char getEdge() {
            return edge;
        }

        public int getPosition() {
            return position;
        }

        public int getTerminal() {
            return terminal;
        }

        public int getHref() {
            return href;
        }

        public int getCompactHref() {
            return compactHref;
        }

        public int getPrevTerminal() {
            return prevTerminal;
        }

        public void setTerminal(int terminal) {
            this.terminal = terminal;
        }

        public void setHref(int href) {
            this.href = href;
        }

        public void setCompactHref(int compactHref) {
            this.compactHref = compactHref;
        }

        public void setPrevTerminal(int prevTerminal) {
            this.prevTerminal = prevTerminal;
        }

        public int getChild(char c) {
            Integer child = children.get(c);
            return child == null ? -1 : child;
        }

       public boolean hasChild(char c) {
            return children.containsKey(c);
       }

        public void addChild(Node child) {
            children.put(child.getEdge(), child.getPosition());
        }

        public int[] getAllChildren() {
            return children.values().stream().mapToInt(i -> i).toArray();
        }
    }
}*/






/*
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class Test {
    private static Random random = new Random();
    private static final int BUF = 5;
    private static final int MAXLENGTH = 3;
    private static final int SIZE = 10;

    public static void main(String[] args) {
        for (int i = 0; i < random.nextInt(SIZE + 2); i++) {
            randomTest(i);
        }
    }


    private static void randomTest(int i) {
        String[] strings = randomStrings(random.nextInt(SIZE));
        StringsTree stringsTree = new StringsTree(strings);
        String example = randomString(MAXLENGTH + random.nextInt(BUF));
        System.out.printf("%20s\n", "Test" + i);
        for (String string : strings) {
            System.out.printf("%-20s\n", string);
        }
        System.out.printf("%-20s\n", example);
        System.out.printf("%20s\n", "Answer" + i);
        for (boolean checker : stringsTree.stringsInExample(example)) {
            System.out.println(checker ? "YES" : "NO");
        }
        System.out.println("--------------------");
    }

    private static String[] randomStrings(int size) {
        String[] strings = new String[size];
        for (int i = 0; i < size; i++) {
            strings[i] = randomString(random.nextInt(MAXLENGTH));
        }
        return strings;
    }

    private static String randomString(int length) {
        StringBuilder sb = new StringBuilder();
        while (sb.length() < length) {
            sb.append(randomChar());
        }
        return sb.toString();
    }

    private static char randomChar() {
        return (char) ('a' + Math.abs((random.nextInt('z' - 'a') + random.nextInt('z' - 'a')) / 2));
    }
}

class StringsTree {
    private StringsTree.Node root;
    private int counter = -1;

    public StringsTree(String[] strings) {
        root = new StringsTree.Node(null, '#');
        makeTree(strings);
    }

    public boolean[] stringsInExample(String string) {
        boolean[] contains = new boolean[counter + 1];
        StringsTree.Node cur = root;
        for (char c : string.toCharArray()) {
            if (cur.isTerminal()) {
                for (int i : cur.getAllTerminals()) {
                    contains[i] = true;
                }
            }

            if (!cur.getCompactHref().isRoot()) {
                StringsTree.Node current = cur.getCompactHref();
                while (!current.isRoot()) {
                    for (int i : current.getAllTerminals()) {
                        contains[i] = true;
                    }
                    current = current.getCompactHref();
                }
            }

            if (cur.hasChild(c)) {
                cur = cur.getChild(c);
            } else {
                cur = getLink(cur, c);
            }
        }
        if (cur.isTerminal()) {
            for (int i : cur.getAllTerminals()) {
                contains[i] = true;
            }
        }
        return contains;
    }

    public boolean contains(String string) {
        StringsTree.Node cur = root;
        for (char c : string.toCharArray()) {
            if (!cur.hasChild(c)) {
                return false;
            }
            cur = cur.getChild(c);
        }
        return true;
    }

    private void makeTree(String[] strings) {
        for (String string : strings) {
            addString(string);
        }
        root.setHref(root);
        root.setCompactHref(root);
        dfs(root);
        bfs();
    }

    private void bfs() {
        Queue<StringsTree.Node> queue = new LinkedList<>();
        queue.add(root);
        while (!queue.isEmpty()) {
            StringsTree.Node current = queue.remove();
            StringsTree.Node[] children = current.getAllChildren();

            for (StringsTree.Node child : children) {
                if (current.isRoot()) {
                    child.setHref(root);
                    child.setCompactHref(root);
                } else {
                    child.setHref(getLink(child.getParent(), child.getEdge()));

                    if (child.getHref().isTerminal() || child.getHref().isRoot()) {
                        child.setCompactHref(child.getHref());
                    } else {
                        child.setCompactHref(child.getHref().getCompactHref());
                    }
                }
                queue.add(child);
            }
        }
    }

    private StringsTree.Node getLink(StringsTree.Node parent, char edge) {
        if (parent.getHref().hasChild(edge)) {
            return parent.getHref().getChild(edge);
        } else if (parent.getHref() == parent.getHref().getHref()) {
            return parent.getHref();
        } else {
            return getLink(parent.getHref(), edge);
        }
    }

    private void dfs(StringsTree.Node root) {
        int[] terminals =  root.getAllTerminals();
        StringsTree.Node[] children = root.getAllChildren();
        for (StringsTree.Node child : children) {
            child.addTerminals(terminals);
            dfs(child);
        }
    }

    private void addString(String string) {
        StringsTree.Node cur = root;
        for (char c : string.toCharArray()) {
            cur = cur.addChild(c);
        }
        cur.addTerminals(++counter);
    }

    private class Node {
        private HashMap<Character, StringsTree.Node> children;
        private StringsTree.Node href;
        private StringsTree.Node compactHref;
        private StringsTree.Node parent;
        private final char edge;
        private ArrayList<Integer> terminals;

        public StringsTree.Node getParent() {
            return parent;
        }

        public Node(StringsTree.Node parent, char edge) {
            this.parent = parent;
            this.edge = edge;
            children = new HashMap<>();
            terminals = new ArrayList<>();
        }

        public boolean isRoot() {
            return edge == '#';
        }

        public boolean isTerminal() {
            return !this.terminals.isEmpty();
        }

        public void addTerminals(int... terminal) {
            for (int i : terminal) {
                terminals.add(i);
            }
        }

        public int[] getAllTerminals() {
            return terminals.stream().mapToInt(i -> i).toArray();
        }

        public StringsTree.Node getChild(char c) {
            return this.children.get(c);
        }

        public StringsTree.Node[] getAllChildren() {
            StringsTree.Node[] nodes = new StringsTree.Node[children.size()];
            int i = -1;
            for (Map.Entry<Character, StringsTree.Node> entry: children.entrySet()) {
                nodes[++i] = entry.getValue();
            }
            return nodes;
        }

        public boolean hasChild(char c) {
            return children.containsKey(c);
        }

        public StringsTree.Node addChild(char c) {
            if (!children.containsKey(c)) {
                StringsTree.Node node = new StringsTree.Node(this, c);
                this.children.put(c, node);
                return node;
            } else {
                return this.children.get(c);
            }
        }

        public void setHref(StringsTree.Node href) {
            this.href = href;
        }

        public void setCompactHref(StringsTree.Node compactHref) {
            this.compactHref = compactHref;
        }

        public StringsTree.Node getHref() {
            return href;
        }

        public char getEdge() {
            return edge;
        }

        public StringsTree.Node getCompactHref() {
            return compactHref;
        }
    }
}

*/
