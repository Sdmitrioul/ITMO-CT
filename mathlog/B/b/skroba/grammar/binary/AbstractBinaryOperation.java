package b.skroba.grammar.binary;

import b.skroba.grammar.Expression;
import b.skroba.grammar.LogicSign;

public abstract class AbstractBinaryOperation implements Expression {
	protected boolean calculated = false;
	protected int hash;
	private final int PRIORITY;
	public final LogicSign sign;
	public Expression first;
	public Expression second;
	
	public AbstractBinaryOperation(int PRIORITY, LogicSign sign, Expression first, Expression second) {
		this.PRIORITY = PRIORITY;
		this.sign = sign;
		this.first = first;
		this.second = second;
	}
	
	@Override
	public boolean isNegation() {
		return false;
	}
	
	@Override
	public int getPriority() {
		return PRIORITY;
	}
	
	@Override
	public int hashCode() {
		if (!calculated) {
			calculated = true;
			hash = 19 + 11 * first.hashCode() + 17 * second.hashCode();
		}
		
		return hash;
	}
	
	@Override
	public String toString() {
		return "(" + first + sign + second + ")";
	}
}
