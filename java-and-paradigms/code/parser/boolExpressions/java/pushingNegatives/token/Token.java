package pushingNegatives.token;

public class Token {
    private TokenType type;
    private char value;

    public Token(TokenType type, char value) {
        this.type = type;
        this.value = value;
    }

    public TokenType getType() {
        return type;
    }

    public char getValue() {
        return value;
    }
}
