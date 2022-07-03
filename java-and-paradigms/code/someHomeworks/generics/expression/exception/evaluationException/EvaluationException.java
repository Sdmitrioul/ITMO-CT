package generics.expression.exception.evaluationException;

import generics.expression.exception.ExpressionException;

public class EvaluationException extends ExpressionException {
    public EvaluationException(String message) {
        super("EvaluationException - " + message);
    }
}
