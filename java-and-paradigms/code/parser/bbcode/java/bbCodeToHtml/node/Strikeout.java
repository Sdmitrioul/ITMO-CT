package bbCodeToHtml.node;

public class Strikeout extends Node {
    public Strikeout(String string) {
        this.node = toHTML(string, "s");
    }
}
