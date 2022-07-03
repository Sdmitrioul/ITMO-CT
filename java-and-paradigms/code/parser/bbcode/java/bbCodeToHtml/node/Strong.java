package bbCodeToHtml.node;

public class Strong extends Node {
    public Strong(String string) {
        this.node = toHTML(string, "strong");
    }
}
