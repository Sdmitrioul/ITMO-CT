package bbCodeToHtml.node;

public class Emphasis extends Node {
    public Emphasis(String string) {
        this.node = toHTML(string, "em");
    }
}
