package b.skroba.axioms;

import b.skroba.grammar.Expression;

import java.util.List;

public class Axiom extends AbstractAxiom {
	private Expression expression;
	
	public Axiom(final Expression expression) {
		this.expression = expression;
	}
	
	@Override
	public String toNaturalOutput(List<Expression> list, int pos) {
		return line(list, expression, pos, "Ax");
	}
}
