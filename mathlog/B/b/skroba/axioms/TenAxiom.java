package b.skroba.axioms;

import b.skroba.grammar.Expression;
import b.skroba.grammar.False;
import b.skroba.grammar.binary.Implication;
import b.skroba.grammar.unary.Negation;
import b.skroba.utils.exception.AxiomException;

import java.util.List;

public class TenAxiom extends AbstractAxiom {
	private final Expression a;
	private final Expression b;
	private final Expression aN;
	private final Expression neg = new False();
	
	// A -> (!A -> B)
	public TenAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			throw new AxiomException("Expected Ten Axiom <A -> (!A -> B)>, got: " + expression);
		}
		
		Implication impl = (Implication) expression;
		
		if (!impl.second.isImplication()) {
			throw new AxiomException("Expected Ten Axiom <A -> (!A -> B)>, got: " + expression);
		}
		
		Implication implSecond = (Implication) impl.second;
		if (!(new Negation(impl.first)).equals(implSecond.first)) {
			throw new AxiomException("Expected Ten Axiom <A -> (!A -> B)>, got: " + expression);
		}
		
		a = impl.first;
		b = implSecond.second;
		aN = new Implication(a, neg);
	}
	
	public static boolean isAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			return false;
		}
		
		Implication impl = (Implication) expression;
		
		if (!impl.second.isImplication()) {
			return false;
		}
		
		Implication implSecond = (Implication) impl.second;
		
		return (new Negation(impl.first)).equals(implSecond.first);
	}
	
	@Override
	public String toNaturalOutput(final List<Expression> list, final int pos) {
		final Expression aNb = new Implication(aN, b);
		StringBuilder sb = new StringBuilder();
		sb.append(line(list, aN, pos + 4, "Ax", a, aN));
		sb.append(line(list, a, pos + 4, "Ax", a, aN));
		sb.append(line(list, neg, pos + 3, "E->", a, aN));
		sb.append(line(list, b, pos + 2, "E_|_", a, aN));
		sb.append(line(list, aNb, pos + 1, "I->", a));
		sb.append(line(list, new Implication(a, aNb), pos, "I->"));
		return sb.toString();
	}
}
