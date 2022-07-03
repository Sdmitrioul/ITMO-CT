package e.skroba.grammar.unary;

import e.skroba.grammar.Expression;
import e.skroba.grammar.Zero;


public final class Number {
	public static Expression getNumber(int value) {
		Expression expression = new Zero();
		for (int i = 0; i < value; i++) {
			expression = new Increment(expression);
		}
		return expression;
	}
}
