package c.skroba.grammar.unary;

import c.skroba.grammar.AbstractExpression;
import c.skroba.grammar.Expression;
import c.skroba.grammar.LogicSign;
import c.skroba.grammar.Variable;

import java.util.Set;

public abstract class AbstractUnaryExpression extends AbstractExpression {
	protected final LogicSign sign;
	public final Expression expression;
	private final int priority;
	private boolean calculated;
	private int hash;
	
	public AbstractUnaryExpression(LogicSign sign, Expression expression, int priority) {
		this.sign = sign;
		this.expression = expression;
		this.priority = priority;
	}
	
	@Override
	public Set<String> getFreeVariables() {
		return expression.getFreeVariables();
	}
	
	@Override
	public boolean isNotFree(String var) {
		return expression.isNotFree(var);
	}
	
	@Override
	public boolean isFreeForVariables(Variable var, Set<String> free, Set<String> close) {
		return expression.isFreeForVariables(var, free, close);
	}
	
	@Override
	public int hashCode() {
		if (!calculated) {
			calculated = true;
			hash = expression.hashCode() * 19 + sign.toString().hashCode();
		}
		return hash;
	}
	
	@Override
	public int getPriority() {
		return priority;
	}
}
