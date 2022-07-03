package b.skroba.grammar.binary;

import b.skroba.grammar.Expression;

import static b.skroba.grammar.LogicSign.IMPLICATION;

public final class Implication extends AbstractBinaryOperation{
	public Implication(final Expression first, final Expression second) {
		super(4, IMPLICATION, first, second);
	}
	
	@Override
	public boolean isImplication() {
		return true;
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
	public boolean equals(Expression expression) {
		if (!expression.isImplication()) {
			return false;
		}
		
		return hashCode() == expression.hashCode();
	}
}
