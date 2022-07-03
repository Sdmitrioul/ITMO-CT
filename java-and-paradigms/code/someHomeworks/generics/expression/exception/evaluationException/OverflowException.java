package generics.expression.exception.evaluationException;

public class OverflowException extends EvaluationException {
    public OverflowException(String operation, String message) {
        super("OverflowException" + " (" + message + ") " + "in operation: " + operation);
    }
}
