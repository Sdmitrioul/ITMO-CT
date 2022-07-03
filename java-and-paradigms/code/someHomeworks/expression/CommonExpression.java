package expression;

import expression.exceptions.ExpressionCalculatingException;

public interface CommonExpression extends TripleExpression {
	int evaluate(int x, int y, int z) throws ExpressionCalculatingException;
}