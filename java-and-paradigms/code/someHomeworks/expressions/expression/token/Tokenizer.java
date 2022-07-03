package expression.token;

import java.util.ArrayList;
import java.util.List;

public class Tokenizer {
    private List<Token> tokens = new ArrayList<>();
    private int curr = -1;
    private String expression;

    public Tokenizer(String expression) {
        this.expression= expression;
        tokens.add(new Token(TokenType.EMPTY, "Begin of expression"));
        tokenize(expression);
        tokens.add(new Token(TokenType.END, "End of expression"));
    }

    public boolean hasNext() {
        return curr < tokens.size() - 1;
    }

    public Token next() {
        return tokens.get(++curr);
    }

    public Token future() {
        return tokens.get(curr + 1);
    }

    public Token prev() {
        return tokens.get(--curr);
    }

    public Token past() {
        return tokens.get(curr - 1);
    }

    public Token current() {
        return tokens.get(curr);
    }

    private void tokenize(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (Character.isWhitespace(s.charAt(i))) {
                continue;
            }
            switch (s.charAt(i)) {
                case '(':
                    tokens.add(new Token(TokenType.LEFT_BR, "("));
                    break;

                case ')':
                    tokens.add(new Token(TokenType.RIGHT_BR, ")"));
                    break;

                case '+':
                    tokens.add(new Token(TokenType.PLUS, "+"));
                    break;

                case '-':
                    tokens.add(new Token(TokenType.MINUS, "-"));
                    break;

                case '*':
                    tokens.add(new Token(TokenType.MUL, "*"));
                    break;

                case '/':
                    tokens.add(new Token(TokenType.DIV, "/"));
                    break;

                case 'm':
                    if (s.charAt(i + 1) == 'i' && s.charAt(i + 2) == 'n' && (!Character.isLetter(s.charAt(i + 3)) && !Character.isDigit(s.charAt(i + 3)))) {
                        i += 2;
                        tokens.add(new Token(TokenType.MIN, "min"));
                    } else if (s.charAt(i + 1) == 'a' && s.charAt(i + 2) == 'x' && (!Character.isLetter(s.charAt(i + 3)) && !Character.isDigit(s.charAt(i + 3)))) {
                        i += 2;
                        tokens.add(new Token(TokenType.MAX, "max"));
                    } else {
                        i += 3;
                        tokens.add(new Token(TokenType.TRASH, s.substring(i - 3, i + 1)));
                    }
                    break;

                case 'c':
                    if (s.charAt(i + 1) == 'o' && s.charAt(i + 2) == 'u' && s.charAt(i + 3) == 'n' && s.charAt(i + 4) == 't' && (!Character.isLetter(s.charAt(i + 5)) && !Character.isDigit(s.charAt(i + 5)))) {
                        i += 4;
                        tokens.add(new Token(TokenType.COUNT, "count"));
                    } else {
                        i += 3;
                        tokens.add(new Token(TokenType.TRASH, s.substring(i - 3, i + 1)));
                    }
                    break;

                case 'x':
                case 'y':
                case 'z':
                    tokens.add(new Token(TokenType.VARIABLE, String.valueOf(s.charAt(i))));
                    break;

                default:
                    if (Character.isDigit(s.charAt(i))) {
                        int j = i;
                        while (j < s.length() && Character.isDigit(s.charAt(j))) {
                            j++;
                        }
                        tokens.add(new Token(TokenType.CONST, s.substring(i, j)));
                        i = j - 1;
                    } else {
                        tokens.add(new Token(TokenType.TRASH, "Trash - " + s.charAt(i)));
                    }

            }
        }
    }
}
