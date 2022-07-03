package generics.expression.exception;

public class ModeException extends ExpressionException {
    public ModeException(String message) {
        super("Incorrect mode: " + message);
    }
}
