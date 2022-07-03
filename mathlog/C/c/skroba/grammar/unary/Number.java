package c.skroba.grammar.unary;

import c.skroba.grammar.Expression;
import c.skroba.grammar.Zero;


public final class Number {
	public static Expression getNumber(int value) {
		Expression expression = new Zero();
		for (int i = 0; i < value; i++) {
			expression = new Increment(expression);
		}
		return expression;
	}
}
