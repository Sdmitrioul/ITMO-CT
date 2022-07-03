package d.skroba.rules;

import d.skroba.grammar.Expression;
import d.skroba.grammar.binary.Implication;
import d.skroba.grammar.calculus.AbstractCalculusExpression;

import java.util.List;
import java.util.Map;

public class WithdrawalRules {
	public static String checkExpression(final Expression expression, final List<Expression> proofed, final Map<Integer, Integer> hashOfProofed) {
		String result = modusPonens(expression, proofed, hashOfProofed);
		
		if (result != null) {
			return result;
		}
		
		result = existRule(expression, proofed, hashOfProofed);
		
		if (result != null) {
			return result;
		}
		
		result = allRules(expression, proofed, hashOfProofed);
		
		return result;
	}
	
	private static String modusPonens(final Expression expression, final List<Expression> proofed, final Map<Integer, Integer> hashOfProofed) {
		for (int i = 0; i < proofed.size(); i++) {
			final Expression source = proofed.get(i);
			if (!source.isImplication()) {
				continue;
			}
			
			final Implication impl = (Implication) source;
			
			if (!impl.right.equals(expression)) {
				continue;
			}
			
			Integer left = hashOfProofed.get(impl.left.hashCode());
			if (left != null) {
				return "M.P. " + left + ", " + (i + 1);
			}
		}
		return null;
	}
	
	private static String existRule(final Expression expression, final List<Expression> proofed, final Map<Integer, Integer> hashOfProofed) {
		if (!expression.isImplication()) {
			return null;
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.left.isOne()) {
			return null;
		}
		
		final AbstractCalculusExpression exp = (AbstractCalculusExpression) impl.left;
		
		Integer result = hashOfProofed.get(new Implication(exp.expression, impl.right).hashCode());
		if (result == null) {
			return null;
		}
		
		if (!impl.right.isNotFree(exp.variable.name)) {
			throw new CheckerException("variable " + exp.variable + " occurs free in ?-rule.");
		}
		
		return "?-intro " + result;
	}
	
	private static String allRules(final Expression expression, final List<Expression> proofed, final Map<Integer, Integer> hashOfProofed) {
		if (!expression.isImplication()) {
			return null;
		}
		
		final Implication impl = (Implication) expression;
		
		if (!impl.right.isAll()) {
			return null;
		}
		
		final AbstractCalculusExpression exp = (AbstractCalculusExpression) impl.right;
		
		Integer result = hashOfProofed.get(new Implication(impl.left, exp.expression).hashCode());
		if (result == null) {
			return null;
		}
		
		if (!impl.left.isNotFree(exp.variable.name)) {
			throw new CheckerException("variable " + exp.variable + " occurs free in @-rule.");
		}
		
		return "@-intro " + result;
	}
}
