package c.skroba.grammar.binary;

import c.skroba.grammar.Expression;

import static c.skroba.grammar.LogicSign.EQUAL;

public final class Equal extends AbstractBinaryExpression {
	public Equal(Expression left, Expression right) {
		super(EQUAL, left, right, 6);
	}
	
	@Override
	public boolean isEquality() {
		return true;
	}
	
	@Override
	public Expression eval(String var, int value) {
		return new Equal(left.eval(var, value), right.eval(var, value));
	}
	
	@Override
	public boolean equals(Expression expression) {
		return expression.isEquality() && expression.hashCode() == hashCode();
	}
}
