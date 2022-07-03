package bbCodeToHtml.node;

public class Underlined extends Node {
    public Underlined(String string) {
        this.node = toHTML(string, "u");
    }
}
