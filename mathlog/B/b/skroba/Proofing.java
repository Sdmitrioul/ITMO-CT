package b.skroba;

import b.skroba.grammar.Expression;

import java.util.List;

public class Proofing {
	public final List<Expression> hypothesis;
	public final Expression expression;
	
	public Proofing(List<Expression> hypothesis, Expression expression) {
		this.hypothesis = hypothesis;
		this.expression = expression;
	}
}
