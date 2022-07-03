package e.skroba.grammar.binary;

import e.skroba.grammar.Expression;
import e.skroba.grammar.LogicSign;

public final class Multiplication extends AbstractBinaryExpression {
	public Multiplication(Expression left, Expression right) {
		super(LogicSign.MUL, left, right, 3);
	}
	
	@Override
	public boolean isMul() {
		return true;
	}
	
	@Override
	public Expression eval(String var, int value) {
		return new Multiplication(left.eval(var, value), right.eval(var, value));
	}
	
	@Override
	public boolean equals(Expression expression) {
		return expression.isMul() && expression.hashCode() == hashCode();
	}
}
