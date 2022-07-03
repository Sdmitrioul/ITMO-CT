import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

public class AhoTree {
    private Node root;
    private ArrayList<Node> nodes = new ArrayList<>();
    private int position = -1;

    public AhoTree(String[] strings) {
        root = new Node(null, -1, 0);
        nodes.add(root);
        for (String string : strings) {
            addString(string);
        }
        makeTree();
    }

    private void makeTree() {
        dfs(root);
    }

    private void dfs(Node current) {
        for (Node node : current.sons) {
            if (node != null) {
                node.leafs.addAll(current.leafs);
                dfs(node);
            }
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

    private Set<Integer> visited(String string) {
        HashSet<Integer> visited = new HashSet<>();
        Node cur = root;
        for (int i = 0; i < string.length(); i++) {
            int c = string.charAt(i) - 'a';
            visited.add(cur.position);
            cur = getLink(cur, c);
        }
        visited.add(cur.position);
        return visited;
    }

    public boolean[] included(String string) {
        boolean[] included = new boolean[position + 1];
        Set<Integer> visited = visited(string);
        for (int i : visited) {
            Node cur = nodes.get(i);
            while (!cur.isRoot()) {
                for (int j : cur.leafs) {
                    included[j] = true;
                }
                cur = getUp(cur);
            }
        }
        return included;
    }

    private class Node {
        Node[] go = new Node[26];
        Node[] sons = new Node[26];
        final Node parent;
        final int symbol;
        final int position;
        Node up;
        Node suffLink;
        HashSet<Integer> leafs = new HashSet<>();

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