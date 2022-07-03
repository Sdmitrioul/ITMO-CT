package a.skroba.grammar.binary;

import a.skroba.grammar.Expression;

import static a.skroba.grammar.LogicSign.CONJUNCTION;

public final class Conjunction extends AbstractBinaryOperation{
	public Conjunction(final Expression first, final Expression second) {
		super(CONJUNCTION, first, second);
	}
}
