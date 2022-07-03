package e.skroba.grammar.binary;

import e.skroba.grammar.Expression;
import e.skroba.grammar.LogicSign;

public final class Conjunction extends AbstractBinaryExpression {
	public Conjunction(Expression left, Expression right) {
		super(LogicSign.CONJUNCTION, left, right, 3);
	}
	
	@Override
	public boolean isConjunction() {
		return true;
	}
	
	@Override
	public Expression eval(String var, int value) {
		return new Conjunction(left.eval(var, value), right.eval(var, value));
	}
	
	@Override
	public boolean equals(Expression expression) {
		return expression.isConjunction() && expression.hashCode() == hashCode();
	}
}
