package d.skroba.grammar.binary;

import d.skroba.grammar.Expression;
import d.skroba.grammar.LogicSign;

public final class Equal extends AbstractBinaryExpression {
	public Equal(Expression left, Expression right) {
		super(LogicSign.EQUAL, left, right, 6);
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
