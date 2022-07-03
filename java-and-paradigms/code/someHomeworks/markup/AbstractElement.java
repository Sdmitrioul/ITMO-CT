package markup;

import java.util.List;

public abstract class AbstractElement implements AbstractUp {
    protected List<AbstractUp> list;

    protected void inMarkDown(StringBuilder str, String razmetka) {
        str.append(razmetka);
        for (AbstractUp exem : list) {
            exem.toMarkdown(str);
        }
        str.append(razmetka);
    }
}
