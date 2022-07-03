package markup;

import java.util.List;

public class Paragraph extends AbstractElement {
    public Paragraph(List<AbstractUp> list) {
        this.list = list;
    }

    @Override
    public void toMarkdown(StringBuilder stringBuilder) {
        inMarkDown(stringBuilder, "");
    }
}
