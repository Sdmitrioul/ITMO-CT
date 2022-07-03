package spoon.token;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Tokenizer {
    private List<Token> tokens = new ArrayList<>();
    private int curr = -1;
    private int countOfInput = 0;
    private String expression;
    private static final Map<String, TokenType> VOCABULARY = Map.of(
            "1", TokenType.PLUS,
            "000", TokenType.MINUS,
            "010", TokenType.NEXT,
            "011", TokenType.PREV,
            "00100", TokenType.BEGIN_WHILE,
            "0011", TokenType.END_WHILE,
            "0010110", TokenType.READ,
            "001010", TokenType.WRITE
    );

    public boolean hasNext() {
        return curr < tokens.size() - 1;
    }

    public Token next() {
        return tokens.get(++curr);
    }

    public Token prev() {
        return tokens.get(--curr);
    }

    public Token current() {
        return tokens.get(curr);
    }

    public int getCountOfInput() {
        return countOfInput;
    }

    public void getBack(int i) {
        curr = i;
    }

    public int getCurr() {
        return curr;
    }

    public int length() {
        return tokens.size();
    }

    public void tokenize(String string) {
        int beginIndex = 0;
        int endIndex = 0;
        StringBuilder stringBuilder = new StringBuilder();
        while (endIndex < string.length()) {
            endIndex++;
            String ccommand = string.substring(beginIndex, endIndex);
            if (VOCABULARY.containsKey(ccommand)) {
                tokens.add(new Token(VOCABULARY.get(ccommand), ccommand));
                if (ccommand.equals("0010110")) {
                    countOfInput++;
                }
                beginIndex = endIndex;
            }
        }
    }

}
