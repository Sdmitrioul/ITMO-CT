package bbCodeToHtml.node;

import java.util.List;

public class ListBB extends Node {
    public ListBB(List<String> list) {
        StringBuilder sb = new StringBuilder();
        for (String entry : list) {
            sb.append(toHTML(entry, "li"));
        }
        this.node = toHTML(sb.toString(), "ul");
    }
}
