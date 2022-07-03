package bbCodeToHtml.token;

import java.util.ArrayList;
import java.util.List;

public class Tokenizer {
    private List<Token> tokens = new ArrayList<>();
    private int curr = -1;

    public void addNewLine() {
        tokens.add(new Token(TokenType.NEW_LINE, "\n"));
    }

    public void addEnd() {
        tokens.add(new Token(TokenType.END, "End"));
    }

    public void deleteLast() {
        tokens.remove(tokens.size() - 1);
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

    public void tokenize(String s) {
        int beginIndex = 0;
        int endIndex = 0;
        while (endIndex < s.length()) {
            if (s.charAt(endIndex) != '[') {
                endIndex++;
            } else {
                tokens.add(new Token(TokenType.TEXT, s.substring(beginIndex, endIndex)));
                beginIndex = endIndex;
                switch (s.substring(beginIndex, beginIndex + 3)) {
                    case "[s]" :
                        tokens.add(new Token(TokenType.STRIKEOUT, s.substring(beginIndex, beginIndex + 3)));
                        beginIndex += 3;
                        endIndex += 3;
                        break;
                    case "[u]" :
                        tokens.add(new Token(TokenType.UNDERLINED, s.substring(beginIndex, beginIndex + 3)));
                        beginIndex += 3;
                        endIndex += 3;
                        break;
                    case "[b]" :
                        tokens.add(new Token(TokenType.STRONG, s.substring(beginIndex, beginIndex + 3)));
                        beginIndex += 3;
                        endIndex += 3;
                        break;
                    case "[i]" :
                        tokens.add(new Token(TokenType.EMPHASIS, s.substring(beginIndex, beginIndex + 3)));
                        beginIndex += 3;
                        endIndex += 3;
                        break;
                    case "[*]" :
                        tokens.add(new Token(TokenType.TEG, s.substring(beginIndex, beginIndex + 3)));
                        beginIndex += 3;
                        endIndex += 3;
                        break;
                    case "[/s" :
                        tokens.add(new Token(TokenType.STRIKEOUT_CLOSE, s.substring(beginIndex, beginIndex + 4)));
                        beginIndex += 4;
                        endIndex += 4;
                        break;
                    case "[/u" :
                        tokens.add(new Token(TokenType.UNDERLINED_CLOSE, s.substring(beginIndex, beginIndex + 4)));
                        beginIndex += 4;
                        endIndex += 4;
                        break;
                    case "[/b" :
                        tokens.add(new Token(TokenType.STRONG_CLOSE, s.substring(beginIndex, beginIndex + 4)));
                        beginIndex += 4;
                        endIndex += 4;
                        break;
                    case "[/i" :
                        tokens.add(new Token(TokenType.EMPHASIS_CLOSE, s.substring(beginIndex, beginIndex + 4)));
                        beginIndex += 4;
                        endIndex += 4;
                        break;
                    case "[li" :
                        tokens.add(new Token(TokenType.LIST, s.substring(beginIndex, beginIndex + 6)));
                        beginIndex += 6;
                        endIndex += 6;
                        break;
                    default:
                        tokens.add(new Token(TokenType.LIST_CLOSE, s.substring(beginIndex, beginIndex + 7)));
                        beginIndex += 7;
                        endIndex += 7;

                }
            }
        }
        if (endIndex != beginIndex) {
            tokens.add(new Token(TokenType.TEXT, s.substring(beginIndex, endIndex)));
        }
    }
}
