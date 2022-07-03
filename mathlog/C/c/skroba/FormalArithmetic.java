package c.skroba;

import c.skroba.grammar.Expression;
import c.skroba.parser.FormalArithmeticParser;
import c.skroba.parser.source.StringSource;
import c.skroba.rules.ArithmeticAxioms;
import c.skroba.rules.LogicAxioms;
import c.skroba.rules.WithdrawalRules;

import java.util.List;
import java.util.Map;

public class FormalArithmetic {
	public static Expression parse(String source) {
		return new FormalArithmeticParser(new StringSource(source)).parse();
	}
	
	public static String checkExpression(final Expression expression, final List<Expression> proofed, final Map<Integer, Integer> hashesProofed) {
		String result = LogicAxioms.getAxiom(expression);
		
		if (result != null) {
			return "Ax. sch. " + result + "] " + expression;
		}
		
		result = ArithmeticAxioms.getAxiom(expression);
		
		if (result != null) {
			return "Ax. " + result + "] " + expression;
		}
		
		result = WithdrawalRules.checkExpression(expression, proofed, hashesProofed);
		
		return result == null ? null : result + "] " + expression;
	}
}
