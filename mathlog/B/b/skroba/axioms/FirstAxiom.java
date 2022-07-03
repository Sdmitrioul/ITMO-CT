package b.skroba.axioms;

import b.skroba.grammar.Expression;
import b.skroba.grammar.binary.Implication;
import b.skroba.utils.exception.AxiomException;

import java.util.List;

public class FirstAxiom extends AbstractAxiom {
	private Expression a;
	private Expression b;
	private Expression lastImpl;
	private Expression expression;
	
	// A -> (B -> A)
	public FirstAxiom(final Expression expression) {
		if (!expression.isImplication()) {
			throw new AxiomException("Expected First Axiom <A -> (B -> A)>, got: " + expression);
		}
		
		Implication impl = (Implication) expression;
		
		if (!impl.second.isImplication()) {
			throw new AxiomException("Expected First Axiom <A -> (B -> A)>, got: " + expression);
		}
		
		Implication implSecond = (Implication) impl.second;
		
		if (!impl.first.equals(implSecond.second)) {
			throw new AxiomException("Expected First Axiom <A -> (B -> A)>, got: " + expression);
		}
		
		a = impl.first;
		b = implSecond.first;
		this.expression = expression;
		lastImpl = implSecond;
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
		
		return impl.first.equals(implSecond.second);
	}
	
	@Override
	public String toNaturalOutput(final List<Expression> list, final int pos) {
		StringBuilder sb = new StringBuilder();
		sb.append(line(list, a, pos + 2, "Ax", a, b));
		sb.append(line(list, lastImpl, pos + 1, "I->", a));
		sb.append(line(list, expression, pos, "I->"));
		return sb.toString();
	}
}
