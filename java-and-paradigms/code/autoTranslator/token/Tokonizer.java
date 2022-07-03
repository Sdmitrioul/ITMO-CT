package autoTranslator.token;

import java.util.ArrayList;
import java.util.List;

public class Tokonizer {
    private List<Token> tokens = new ArrayList<>();
    private int curr = -1;

    public boolean hasNext() {
        return curr < tokens.size() - 1;
    }

    public Token next() {
        return tokens.get(++curr);
    }

    public Token prev() {
        return tokens.get(--curr);
    }

    public void previous() {
        --curr;
    }

    public Token current() {
        return tokens.get(curr);
    }

    public Tokonizer toBegin() {
        curr = -1;
        return this;
    }

    public void returnBack(int i) {
        curr -= i;
    }

    public void returnFuture(int i) {
        curr += i;
    }

    public void addNewLine() {
        tokens.add(new Token(TokenType.NEW_LINE, "\n"));
    }

    public void tokenize(String s) {
        int index = 0;
        while (index < s.length()) {
            char c = s.charAt(index);
            if (partWord(c)) {
                int beginIndex = index;
                index++;
                while (index < s.length() && partWord(s.charAt(index))) {
                    index++;
                }
                tokens.add(new Token(TokenType.WORD, s.substring(beginIndex, index).toLowerCase()));
            } else if (c == '?' || c == '!' || c == '.' || c == ',') {
                tokens.add(new Token(TokenType.PUNCTUATION_SIGN, Character.toString(c)));
                index++;
            } else if (Character.getType(c) == Character.DASH_PUNCTUATION){
                tokens.add(new Token(TokenType.DASH, Character.toString(c)));
                index++;
            } else {
                index++;
            }
        }
    }

    private boolean partWord(char c) {
        return Character.isLetter(c) ||  c == '\'';
    }
}
