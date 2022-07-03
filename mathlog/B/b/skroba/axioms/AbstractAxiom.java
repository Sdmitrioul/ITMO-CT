package b.skroba.axioms;

import b.skroba.grammar.Expression;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public abstract class AbstractAxiom implements Axioms {
	@Override
	public String line(List<Expression> list,  Expression expression, int pos, String move, Expression... hypothesis) {
		return "[" + pos + "] " +
						concatHypothesis(list, hypothesis) + "|-" +
						expression + " [" + move + "]\n" ;
	}
	
	public String concatHypothesis(List<Expression> list, Expression... hypothesis) {
		if (list.size() == 0) {
			return Arrays.stream(hypothesis).map(Expression::toString).collect(Collectors.joining(","));
		}
		
		if (hypothesis.length == 0) {
			return list.stream().map(Expression::toString).collect(Collectors.joining(","));
		}
		
		return list.stream().map(Expression::toString).collect(Collectors.joining(",")) + "," + Arrays.stream(hypothesis).map(Expression::toString).collect(Collectors.joining(","));
	}
}
