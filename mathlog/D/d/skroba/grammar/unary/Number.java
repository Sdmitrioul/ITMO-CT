package d.skroba.grammar.unary;

import d.skroba.grammar.Expression;
import d.skroba.grammar.Zero;


public final class Number {
	public static Expression getNumber(int value) {
		Expression expression = new Zero();
		for (int i = 0; i < value; i++) {
			expression = new Increment(expression);
		}
		return expression;
	}
}
