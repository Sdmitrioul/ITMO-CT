package autoTranslator;

import autoTranslator.token.Token;
import autoTranslator.token.TokenType;
import autoTranslator.token.Tokonizer;

import java.util.Map;

public class Parser {
    private Tokonizer tokens;
    private StringBuilder text;
    private final Map<String, Map<Tokonizer, String>> vocabulary; //Map<first word, Map<all words, translation>>

    public Parser(Tokonizer tokens, Map<String, Map<Tokonizer, String>> vocabulary) {
        this.tokens = tokens;
        this.vocabulary = vocabulary;
        text = new StringBuilder();
        parse();
    }

    private void parse() {
        while (tokens.hasNext()) {
            Token token = tokens.next();
            if (token.getType() == TokenType.PUNCTUATION_SIGN) {
                text.deleteCharAt(text.length() - 1);
                text.append(token.getValue()).append(" ");
            } else if (token.getType() == TokenType.NEW_LINE) {
                text.deleteCharAt(text.length() - 1);
                text.append(token.getValue());
            } else if (vocabulary.get(token.getValue()) == null) {
                text.append('\"').append(token.getValue()).append("\"").append(" ");
            } else {
                tokens.previous();
                int length = 0;
                String translation = "DEF@ULT";
                for (Map.Entry<Tokonizer, String> entry: vocabulary.get(token.getValue()).entrySet()) {
                    int i = 0;
                    Tokonizer tokonizer = entry.getKey();
                    boolean proverka = true;
                    while (tokonizer.hasNext() && tokens.hasNext()) {
                        if (tokonizer.next().getType() == tokens.next().getType() && tokonizer.current().getValue().equals(tokens.current().getValue())) {
                            i++;
                        } else {
                            proverka = false;
                            i++;
                            break;
                        }
                    }
                    if (tokonizer.hasNext())
                        proverka = false;
                    if (proverka) {
                        if (length < i){
                            translation = entry.getValue();
                            length = i;
                        }
                    }
                    tokonizer.toBegin();
                    tokens.returnBack(i);
                }
                text.append(translation).append(" ");
                tokens.returnFuture(length);
            }
        }
    }

    public String getText() {
        return text.toString();
    }
}
