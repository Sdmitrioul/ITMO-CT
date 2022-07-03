package d.skroba.grammar.unary;

import d.skroba.grammar.Expression;
import d.skroba.grammar.Variable;
import d.skroba.grammar.LogicSign;

public final class Increment extends AbstractUnaryExpression {
	public Increment(Expression expression) {
		super(LogicSign.INCREMENT, expression, 1);
	}
	
	@Override
	public String toString(boolean debug) {
		return expression.getPriority() > getPriority()
				? "(" + expression.toString(debug) + ")" + sign
				: expression.toString(debug) + sign;
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
