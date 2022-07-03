package generics.expression.exception.evaluationException;

public class DivisionByZeroException extends EvaluationException {
    public DivisionByZeroException(String operation) {
        super("DivisionByZeroException: " + operation);
    }
}
