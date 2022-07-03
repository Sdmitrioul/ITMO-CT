package markup;

import java.util.List;

public class Strikeout extends AbstractElement {
    public Strikeout(List<AbstractUp> list) {
        this.list = list;
    }

    @Override
    public void toMarkdown(StringBuilder stringBuilder) {
        inMarkDown(stringBuilder, "~" );
    }
}
