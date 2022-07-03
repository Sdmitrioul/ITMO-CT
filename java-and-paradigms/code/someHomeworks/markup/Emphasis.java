package markup;

import java.util.List;

public class Emphasis extends AbstractElement {
    public Emphasis(List<AbstractUp> list) {
        this.list = list;
    }

    @Override
    public void toMarkdown(StringBuilder stringBuilder) {
        inMarkDown(stringBuilder, "*" );
    }
}
