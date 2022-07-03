package a.skroba.grammar.unary;

import a.skroba.grammar.Expression;

import static a.skroba.grammar.LogicSign.NEGATION;

/**
 * Negation class extends {@link AbstractUnaryOperation}.
 */
public final class Negation extends AbstractUnaryOperation {
	public Negation(final Expression expression) {
		super(NEGATION, expression);
	}
}
