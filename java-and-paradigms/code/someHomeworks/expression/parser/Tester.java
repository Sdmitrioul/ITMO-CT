package expression.parser;

import expression.TripleExpression;
import expression.exceptions.ExpressionCalculatingException;
import expression.exceptions.ExpressionParser;
import expression.exceptions.ExpressionParsingException;


public class Tester {
	public static void main(String[] args) throws ExpressionParsingException, ExpressionCalculatingException {
		String aka_fast = "(x -2)   *10 - z /10";
		ExpressionParser fast = new ExpressionParser();
		TripleExpression a = fast.parse(aka_fast);

		System.out.println(a.evaluate(5, 0, 0));
	}
}