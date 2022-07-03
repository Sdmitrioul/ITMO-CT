package a.skroba.grammar.binary;

import a.skroba.grammar.Expression;

import static a.skroba.grammar.LogicSign.DISJUNCTION;

public final class Disjunction extends AbstractBinaryOperation {
	public Disjunction(final Expression first, final Expression second) {
		super(DISJUNCTION, first, second);
	}
}
