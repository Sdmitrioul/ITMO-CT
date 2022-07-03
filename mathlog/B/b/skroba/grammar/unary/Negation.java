package b.skroba.grammar.unary;

import b.skroba.grammar.Expression;

import static b.skroba.grammar.LogicSign.NEGATION;

/**
 * Negation class extends {@link AbstractUnaryOperation}.
 */
public final class Negation extends AbstractUnaryOperation {
	public Negation(Expression expression) {
		super(1, NEGATION, expression);
	}
	
	@Override
	public boolean isNegation() {
		return true;
	}
	
	@Override
	public int hashCode() {
		if (!calculated) {
			calculated = true;
			hash = 31 * expression.hashCode() + 13;
		}
		
		return hash;
	}
	
	@Override
	public boolean equals(Expression expression) {
		if (!expression.isNegation()) {
			return false;
		}
		
		return expression.hashCode() == hashCode();
	}
	
	@Override
	public String toString() {
		return "(" + expression + "->_|_)";
	}
}
