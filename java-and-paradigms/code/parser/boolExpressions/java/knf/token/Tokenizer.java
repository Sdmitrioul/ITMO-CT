package knf.token;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Tokenizer {
    private List<Token> tokens = new ArrayList<>();
    private int curr = -1;
    private int currvariable = 0;
    private String expression;
    private Map<Integer, Character> variables = new HashMap<>();
    private static final Map<Integer, Character> assosiation = Map.of(
            0, 'a',
            1, 'b',
            2, 'c',
            3, 'd',
            4, 'e',
            5, 'f',
            6, 'g',
            7, 'h',
            8, 'i',
            9, 'j'

    );

    public Tokenizer(String expression) {
        this.expression = expression;
        tokenize(expression);
        tokens.add(new Token(TokenType.END, 'E'));
    }

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

    public Map<Integer, Character> getVariables() {
        return variables;
    }

    public int getCurrvariable() {
        return currvariable;
    }

    private void tokenize(String string) {
        for (int i = 0; i < string.length(); i++) {
            if (Character.isWhitespace(string.charAt(i))) {
                continue;
            }
            switch (string.charAt(i)) {
                case ('0'):
                    tokens.add(new Token(TokenType.FALSE, '0'));
                    break;
                case ('1'):
                    tokens.add(new Token(TokenType.TRUE, '1'));
                    break;
                case ('~'):
                    tokens.add(new Token(TokenType.NEGATION, '~'));
                    break;
                case ('|'):
                    tokens.add(new Token(TokenType.DISJUNCTION, '|'));
                    break;
                case ('&'):
                    tokens.add(new Token(TokenType.CONJUNCTION, '&'));
                    break;
                case ('('):
                    tokens.add(new Token(TokenType.LEFT_BR, '('));
                    break;
                case (')'):
                    tokens.add(new Token(TokenType.RIGHT_BR, ')'));
                    break;
                default:
                    if (!variables.containsValue(string.charAt(i))) {
                        variables.put(currvariable, string.charAt(i));
                        tokens.add(new Token(TokenType.VARIABLE, assosiation.get(currvariable++)));
                    } else {
                        int tmp = currvariable - 1;
                        while (variables.get(tmp) != string.charAt(i)) {
                            tmp--;
                        }
                        tokens.add(new Token(TokenType.VARIABLE, assosiation.get(tmp)));
                    }
                    break;
            }
        }
    }
}
