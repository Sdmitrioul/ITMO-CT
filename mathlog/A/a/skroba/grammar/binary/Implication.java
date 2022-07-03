package a.skroba.grammar.binary;

import a.skroba.grammar.Expression;

import static a.skroba.grammar.LogicSign.IMPLICATION;

public final class Implication extends AbstractBinaryOperation{
	public Implication(final Expression first, final Expression second) {
		super(IMPLICATION, first, second);
	}
}
