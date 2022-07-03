package b.skroba.axioms;

import b.skroba.grammar.Expression;
import b.skroba.grammar.binary.Implication;
import b.skroba.utils.exception.AxiomException;

import java.util.List;

public class SecondAxiom extends AbstractAxiom {
	private Expression a;
	private Expression b;
	private Expression c;
	private Implication expression;
	
	// (A -> B) -> (A -> B -> C) -> (A -> C)
	public SecondAxiom(Expression expression) {
		if (!expression.isImplication()) {
			throw new AxiomException("Expected Second Axiom <(A -> B) -> (A -> B -> C) -> (A -> C)>, got: " + expression);
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.first.isImplication() || !impl.second.isImplication()) {
			throw new AxiomException("Expected Second Axiom <(A -> B) -> (A -> B -> C) -> (A -> C)>, got: " + expression);
		}
		
		final Implication implLeft = (Implication) impl.first;
		final Implication implRight = (Implication) impl.second;
		
		if (!implRight.second.isImplication() || !implRight.first.isImplication()) {
			throw new AxiomException("Expected Second Axiom <(A -> B) -> (A -> B -> C) -> (A -> C)>, got: " + expression);
		}
		
		final Implication implLast = (Implication) implRight.second;
		
		a = implLeft.first;
		b = implLeft.second;
		c = implLast.second;
		this.expression = impl;
	}
	
	// (A -> B) -> (A -> B -> C) -> (A -> C)
	public static boolean isAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			return false;
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.first.isImplication() || !impl.second.isImplication()) {
			return false;
		}
		
		final Implication implLeft = (Implication) impl.first;
		final Implication implRight = (Implication) impl.second;
		
		if (!implRight.second.isImplication() || !implRight.first.isImplication()) {
			return false;
		}
		
		final Implication implLast = (Implication) implRight.second;
		final Expression a = implLeft.first;
		final Expression b = implLeft.second;
		final Expression c = implLast.second;
		
		return implRight.first.equals(new Implication(a, new Implication(b, c)));
	}
	
	// [i+5] (A -> B), (A -> B -> C), A |- A -> B -> C [Ax]
	// [i+5] (A -> B), (A -> B -> C), A |- A [Ax]
	// [i+4] (A -> B), (A -> B -> C), A |- B -> C [E->]
	// [i+5] (A -> B), (A -> B -> C), A |- A -> B [Ax]
	// [i+5] (A -> B), (A -> B -> C), A |- A [Ax]
	// [i+4] (A -> B), (A -> B -> C), A |- B [E->]
	// [i+3] (A -> B), (A -> B -> C), A |- C [E->]
	// [i+2] (A -> B), (A -> B -> C) |- (A -> C) [I->]
	// [i+1] (A -> B) |- (A -> B -> C) -> (A -> C) [I->]
	// [ i ] |- (A -> B) -> (A -> B -> C) -> (A -> C) [I->]
	@Override
	public String toNaturalOutput(final List<Expression> list, final int pos) {
		Implication ab = new Implication(a, b);
		Implication abc = new Implication(a, new Implication(b, c));
		StringBuilder sb = new StringBuilder();
		sb.append(line(list, abc, pos + 5, "Ax", ab, a, abc));
		sb.append(line(list, a, pos + 5, "Ax", abc, ab, a));
		sb.append(line(list, new Implication(b, c), pos + 4, "E->", abc, ab, a));
		sb.append(line(list, ab, pos + 5, "Ax", abc,ab, a));
		sb.append(line(list, a, pos + 5, "Ax", abc, ab, a));
		sb.append(line(list, b, pos + 4, "E->", abc, ab, a));
		sb.append(line(list, c, pos + 3, "E->", abc, ab, a));
		sb.append(line(list, new Implication(a, c), pos + 2, "I->", abc, ab));
		sb.append(line(list, expression.second, pos + 1, "I->", ab));
		sb.append(line(list, expression, pos, "I->"));
		return sb.toString();
	}
}
