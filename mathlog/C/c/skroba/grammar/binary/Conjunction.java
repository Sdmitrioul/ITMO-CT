package c.skroba.grammar.binary;

import c.skroba.grammar.Expression;

import static c.skroba.grammar.LogicSign.CONJUNCTION;

public final class Conjunction extends AbstractBinaryExpression {
	public Conjunction(Expression left, Expression right) {
		super(CONJUNCTION, left, right, 3);
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
