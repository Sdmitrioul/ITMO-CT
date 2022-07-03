package bbCodeToHtml.node;

public abstract class Node {
    protected String node;

    protected String toHTML(String string, String node) {
        StringBuilder sb = new StringBuilder();
        sb.append('<').append(node).append('>');
        sb.append(string).append("</").append(node).append('>');
        return sb.toString();
    }

    @Override
    public String toString() {
        return node;
    }
}
