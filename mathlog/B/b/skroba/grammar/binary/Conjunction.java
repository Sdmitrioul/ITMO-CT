package b.skroba.grammar.binary;

import b.skroba.grammar.Expression;

import static b.skroba.grammar.LogicSign.CONJUNCTION;

public final class Conjunction extends AbstractBinaryOperation{
	public Conjunction(final Expression first, final Expression second) {
		super(2, CONJUNCTION, first, second);
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
		return true;
	}
	
	@Override
	public boolean equals(Expression expression) {
		if (!expression.isConjunction()) {
			return false;
		}
		boolean res = hashCode() == expression.hashCode();
		return res;
	}
}
