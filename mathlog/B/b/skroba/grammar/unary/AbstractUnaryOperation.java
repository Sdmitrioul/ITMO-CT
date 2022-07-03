package b.skroba.grammar.unary;

import b.skroba.grammar.Expression;
import b.skroba.grammar.LogicSign;

public abstract class AbstractUnaryOperation implements Expression {
	protected boolean calculated = false;
	protected int hash;
	private final int PRIORITY;
	public final LogicSign sign;
	public final Expression expression;
	
	public AbstractUnaryOperation(int PRIORITY, LogicSign sign, Expression expression) {
		this.PRIORITY = PRIORITY;
		this.sign = sign;
		this.expression = expression;
	}
	
	@Override
	public boolean isImplication() {
		return false;
	}
	
	@Override
	public boolean isDisjunction() {
		return false;
	}
	
	@Override
	public boolean isConjunction() {
		return false;
	}
	
	@Override
	public int getPriority() {
		return PRIORITY;
	}
	
	@Override
	public String toString() {
		return "!(" + expression + ")";
	}
}
