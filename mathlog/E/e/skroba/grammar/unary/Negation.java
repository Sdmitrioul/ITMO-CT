package e.skroba.grammar.unary;

import e.skroba.grammar.Expression;
import e.skroba.grammar.Variable;
import e.skroba.grammar.LogicSign;

public final class Negation extends AbstractUnaryExpression {
	public Negation(Expression expression) {
		super(LogicSign.NEGATION, expression, 2);
	}
	
	@Override
	public String toString(boolean debug) {
		return expression.getPriority() > getPriority()
				? sign + "(" + expression.toString(debug) + ")"
				: sign + expression.toString(debug);
	}
	
	@Override
	public Expression eval(String var, int value) {
		return new Negation(expression.eval(var, value));
	}
	
	@Override
	public Expression diff(Variable var, Expression comparing) {
		if (!comparing.isNegation()) {
			return null;
		}
		
		final Negation neg = (Negation) comparing;
		return expression.diff(var, neg.expression);
	}
	
	@Override
	public boolean isNegation() {
		return true;
	}
	
	@Override
	public boolean equals(Expression expression) {
		return expression.isNegation() && expression.hashCode() == hashCode();
	}
}
