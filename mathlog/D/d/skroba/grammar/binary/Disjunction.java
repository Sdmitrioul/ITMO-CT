package d.skroba.grammar.binary;

import d.skroba.grammar.Expression;
import d.skroba.grammar.LogicSign;

public final class Disjunction extends AbstractBinaryExpression{
	public Disjunction(Expression left, Expression right) {
		super(LogicSign.DISJUNCTION, left, right, 4);
	}
	
	@Override
	public boolean isDisjunction() {
		return true;
	}
	
	@Override
	public Expression eval(String var, int value) {
		return new Disjunction(left.eval(var, value), right.eval(var, value));
	}
	
	@Override
	public boolean equals(Expression expression) {
		return expression.isDisjunction() && expression.hashCode() == hashCode();
	}
}
