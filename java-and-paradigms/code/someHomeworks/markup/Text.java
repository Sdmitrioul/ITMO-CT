package markup;

public class Text extends AbstractElement{
    protected String string;

    public Text(String string) {
        this.string = string;
    }

    @Override
    public void toMarkdown(StringBuilder stringBuilder) {
        stringBuilder.append(string);
    }
}
