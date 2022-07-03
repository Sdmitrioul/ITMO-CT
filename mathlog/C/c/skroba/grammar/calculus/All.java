package c.skroba.grammar.calculus;

import c.skroba.grammar.Empty;
import c.skroba.grammar.Expression;
import c.skroba.grammar.Variable;

import static c.skroba.grammar.LogicSign.ALL;

public final class All extends AbstractCalculusExpression {
	public All(Variable var, Expression expression) {
		super(ALL, var, expression, 6);
	}
	
	@Override
	public boolean isAll() {
		return true;
	}
	
	@Override
	public Expression eval(String var, int value) {
		return var.equals(variable.name) ? expression.eval(var, value) : new All(variable, expression.eval(var, value));
	}
	
	@Override
	public Expression diff(Variable var, Expression comparing) {
		if (var.equals(variable) && comparing.equals(this)) {
			return new Empty();
		}
		
		if (!comparing.isAll()) {
			return null;
		}
		
		final All all = (All) comparing;
		return all.variable.equals(variable) && !variable.equals(var) ? expression.diff(var, all.expression) : null;
	}
	
	@Override
	public boolean equals(Expression expression) {
		return expression.isAll() && expression.hashCode() == hashCode();
	}
}
