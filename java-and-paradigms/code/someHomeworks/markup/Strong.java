package markup;

import java.util.List;

public class Strong extends AbstractElement {
    public Strong(List<AbstractUp> list) {
        this.list = list;
    }

    @Override
    public void toMarkdown(StringBuilder stringBuilder) {
        inMarkDown(stringBuilder, "__" );
    }
}
