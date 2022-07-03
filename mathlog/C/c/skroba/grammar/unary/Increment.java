package c.skroba.grammar.unary;

import c.skroba.grammar.Expression;
import c.skroba.grammar.Variable;

import static c.skroba.grammar.LogicSign.INCREMENT;

public final class Increment extends AbstractUnaryExpression {
	public Increment(Expression expression) {
		super(INCREMENT, expression, 1);
	}
	
	@Override
	public String toString(boolean debug) {
		return expression.toString(debug) + sign;
	}
	
	@Override
	public Expression eval(String var, int value) {
		return new Increment(expression.eval(var, value));
	}
	
	@Override
	public Expression diff(Variable var, Expression comparing) {
		if (!comparing.isIncrement()) {
			return null;
		}
		
		final Increment inc = (Increment) comparing;
		return expression.diff(var, inc.expression);
	}
	
	@Override
	public boolean isIncrement() {
		return true;
	}
	
	@Override
	public boolean equals(Expression expression) {
		return expression.isIncrement() && hashCode() == expression.hashCode();
	}
}
