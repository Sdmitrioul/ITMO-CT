package b.skroba.grammar.binary;

import b.skroba.grammar.Expression;

import static b.skroba.grammar.LogicSign.DISJUNCTION;

public final class Disjunction extends AbstractBinaryOperation {
	public Disjunction(final Expression first, final Expression second) {
		super(3, DISJUNCTION, first, second);
	}
	
	@Override
	public boolean isImplication() {
		return false;
	}
	
	@Override
	public boolean isDisjunction() {
		return true;
	}
	
	@Override
	public boolean isConjunction() {
		return false;
	}
	
	@Override
	public boolean equals(Expression expression) {
		if (!expression.isDisjunction()) {
			return false;
		}
		return hashCode() == expression.hashCode();
	}
}
