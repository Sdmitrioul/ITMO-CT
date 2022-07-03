package e.skroba.generator;

import e.skroba.grammar.Expression;
import e.skroba.grammar.binary.Conjunction;
import e.skroba.grammar.unary.Negation;
import e.skroba.util.Writer;

public class GeneratorForThreeImpl extends AbstractGenerator {
	public GeneratorForThreeImpl(Writer writer) {
		super(writer);
	}
	
	@Override
	protected void generateFirstLine(int num) {
		if (num % 3 == 0) {
			writer.write("|-" + exist(X, equality(num(num), mul(THREE, X))));
			return;
		}
		
		writer.write("|-" + all(X, neg(equality(num(num), mul(THREE, X)))));
	}
	
	@Override
	protected void generateLogic(int numberFor) {
		if (numberFor % 3 == 0) {
			generateLogicForModZero(numberFor);
			return;
		}
		
		generateNotZeroModLogic(numberFor);
	}
	
	private void generateNotZeroModLogic(int numberFor) {
		final Expression res = neg(equality(num(numberFor), mul(THREE, X)));
		generateStart();
		generateLogicNotZeroModRecursion(numberFor);
		generateAll(res);
	}
	
	private void generateLogicNotZeroModRecursion(int numberFor) {
		if (numberFor < 0) {
			writer.write(seventhAxiom(THREE));
			return;
		}
		
		generateLogicNotZeroModRecursion(numberFor - 3);
		forwardPart(numberFor);
		middlePart(numberFor);
		backwardPart(numberFor);
	}
	
	private void backwardPart(final int numberFor) {
		final Expression NUM = num(numberFor);
		final Expression three = mul(THREE, X);
		final Expression incThree = mul(THREE, inc(X));
		final Expression zero = neg(equality(num(numberFor), mul(THREE, ZERO)));
		final Expression midRes = neg(equality(num(numberFor), incThree));
		final Expression res = neg(equality(num(numberFor), three));
		final Expression buf = numberFor > 3 ? neg(equality(num(numberFor - 3), three)) : fourthAxiom(numberFor == 2 ? three : inc(three));
		
		if (!(buf instanceof Negation)) {
			throw new RuntimeException("Something wrong");
		}
		
		final Expression insideBuf = ((Negation) buf).expression;
		
		writer.write(buf);
		generateFirstAxiomAndMPOnes(buf, equality(NUM, incThree));
		generateNinthAxiomMP(equality(NUM, incThree), insideBuf);
		generateFirstAxiomAndMPOnes(midRes, res);
		generateAll(impl(res, midRes));
		generateThirdAxiomMP(zero, all(X, impl(res, midRes)));
		generateInductionAxiomMP(zero, all(X, impl(res, midRes)), res);
	}
	
	private void middleONE() {
		final Expression three = mul(THREE, X);
		final Expression ptThree = mul(THREE, inc(X));
		final Expression oneToThree = equality(ONE, ptThree);
		final Expression threeToOne = equality(ptThree, ONE);
		
		writer.write(thirdAxiom(inc(three, 2), ZERO));
		middleEndingForMod(three, ptThree, oneToThree, threeToOne, ONE);
		generateFirstAxiomAndMPOnes(impl(equality(inc(three, 3), ONE), equality(inc(three, 2), ZERO)), threeToOne);
		generateSecondAxiomMP(threeToOne, equality(inc(three, 3), ONE), equality(inc(three, 2), ZERO));
		generateFirstAxiomAndMPOnes(impl(threeToOne, equality(inc(three, 2), ZERO)), oneToThree);
		generateSecondAxiomMP(oneToThree, threeToOne, equality(inc(three, 2), ZERO));
	}
	
	private void middleTWO() {
		final Expression three = mul(THREE, X);
		final Expression ptThree = mul(THREE, inc(X));
		final Expression twoToThree = equality(TWO, ptThree);
		final Expression threeToTWO = equality(ptThree, TWO);
		final Expression zero = equality(inc(three), ZERO);
		
		writer.write(thirdAxiom(inc(three, 1), ZERO));
		writer.write(thirdAxiom(inc(three, 2), ONE));
		
		generateFirstAxiomAndMPOnes(impl(equality(inc(three, 2), ONE), zero), equality(inc(three, 3), TWO));
		generateSecondAxiomMP(equality(inc(three, 3), TWO), equality(inc(three, 2), ONE), zero);
		
		middleEndingForMod(three, ptThree, twoToThree, threeToTWO, TWO);
		
		generateFirstAxiomAndMPOnes(impl(equality(inc(three, 3), TWO), zero), threeToTWO);
		
		generateSecondAxiomMP(threeToTWO, equality(inc(three, 3), TWO), zero);
		generateFirstAxiomAndMPOnes(impl(threeToTWO, zero), twoToThree);
		generateSecondAxiomMP(twoToThree, threeToTWO, zero);
	}
	
	private void middleEndingForMod(Expression three, Expression ptThree, Expression twoToThree, Expression threeToTWO, Expression two) {
		writer.write(secondAxiom(ptThree, inc(three, 3), two));
		writer.write(impl(threeToTWO, equality(inc(three, 3), two)));
		writer.write(secondAxiom(two, ptThree, two));
		
		generateFirstAxiomAndMPOnes(equality(two, two), twoToThree);
		generateSecondAxiomMP(twoToThree, equality(two, two), threeToTWO);
	}
	
	private void middlePart(final int numberFor) {
		if (numberFor == 1) {
			middleONE();
			return;
		} else if (numberFor == 2) {
			middleTWO();
			return;
		}
		
		final Expression NUM = num(numberFor);
		final Expression three = mul(THREE, X);
		final Expression ptThree = mul(THREE, inc(X));
		final Expression curToThree = equality(NUM, ptThree);
		final Expression threeToCur = equality(ptThree, NUM);
		final Expression decTwo = equality(num(numberFor - 2), inc(three, 1));
		final Expression decThree = equality(num(numberFor - 3), three);
		final Expression decOne = equality(num(numberFor - 1), inc(three, 2));
		final Expression cur = equality(NUM, inc(three, 3));
		final Expression dubThree = equality(ptThree, inc(three, 3));
		
		writer.write(secondAxiom(ptThree, NUM, inc(three, 3)));
		
		writer.write(thirdAxiom(num(numberFor - 1), inc(three, 2)));
		writer.write(thirdAxiom(num(numberFor - 2), inc(three, 1)));
		writer.write(thirdAxiom(num(numberFor - 3), three));
		
		generateFirstAxiomAndMPOnes(impl(decTwo, decThree), decOne);
		generateSecondAxiomMP(decOne, decTwo, decThree);
		generateFirstAxiomAndMPOnes(impl(decOne, decThree), cur);
		generateSecondAxiomMP(cur, decOne, decThree);
		
		writer.write(secondAxiom(NUM, ptThree, NUM));
		
		generateFirstAxiomAndMPOnes(equality(NUM, NUM), curToThree);
		generateSecondAxiomMP(curToThree, equality(NUM, NUM), threeToCur);
		
		writer.write(secondAxiom(ptThree, NUM, inc(three, 3)));
		
		generateFirstAxiomAndMPOnes(dubThree, threeToCur);
		generateSecondAxiomMP(threeToCur, dubThree, cur);
		generateFirstAxiomAndMPOnes(impl(threeToCur, cur), curToThree);
		generateSecondAxiomMP(curToThree, threeToCur, cur);
		generateFirstAxiomAndMPOnes(impl(cur, decThree), curToThree);
		generateSecondAxiomMP(curToThree, cur, decThree);
	}
	
	private void forwardPart(final int numberFor) {
		final Expression NUM = num(numberFor);
		final Expression three = mul(3, 0);
		final Expression eq = equality(three, ZERO);
		
		generateAA(NUM);
		generateFirstAxiomAndMPOnes(eq, equality(three, NUM));
		writer.write(secondAxiom(three, NUM, ZERO));
		generateSecondAxiomMP(equality(three, NUM), eq, equality(NUM, ZERO));
		generateFirstAxiomAndMPOnes(impl(equality(three, NUM), eq), equality(NUM, three));
		writer.write(secondAxiom(NUM, three, NUM));
		generateFirstAxiomAndMPOnes(equality(NUM, NUM), equality(NUM, three));
		generateSecondAxiomMP(equality(NUM, three), equality(NUM, NUM), equality(three, NUM));
		generateFirstAxiomAndMPOnes(impl(equality(three, NUM), equality(NUM, ZERO)), equality(NUM, three));
		generateSecondAxiomMP(equality(NUM, three), equality(three, NUM), equality(NUM, ZERO));
		
		final Expression notEight = fourthAxiom(numberFor);
		
		writer.write(notEight);
		generateFirstAxiomAndMPOnes(notEight, equality(NUM, three));
		generateNinthAxiomMP(equality(NUM, three), equality(NUM, ZERO));
	}
	
	private void generateStart() {
		writer.write(sixAxiom(mul(THREE, X)));
		
		generateStartPart(mul(THREE, X), ZERO, mul(THREE, X));
		generateStartPart(mul(THREE, X), ONE, inc(mul(THREE, X)));
		generateStartPart(mul(THREE, X), TWO, inc(inc(mul(THREE, X))));
		
		generateAA(mul(THREE, inc(X)));
		writer.write(eightAxiom(THREE, X));
		reverseAB(mul(THREE, inc(X)), sum(mul(THREE, X), THREE));
		generateAA(inc(inc(inc(mul(THREE, X)))));
		generateSecondAxiom(sum(mul(THREE, X), THREE), inc(inc(inc(mul(THREE, X)))), mul(THREE, inc(X)));
		reverseAB(inc(inc(inc(mul(THREE, X)))), mul(THREE, inc(X)));
	}
	
	private void generateStartPart(final Expression a, final Expression b, final Expression c) {
		generateAA(sum(a, inc(b)));
		writer.write(firstAxiom(sum(a, b), c));
		writer.write(equality(inc(sum(a, b)), inc(c)));
		writer.write(fifthAxiom(a, b));
		reverseAB(sum(a, inc(b)), inc(sum(a, b)));
		generateSecondAxiom(inc(sum(a, b)), sum(a, inc(b)), inc(c));
	}
	
	private void generateLogicForModZero(final int numberFor) {
		final Expression res = exist(X, equality(num(numberFor), mul(THREE, X)));
		
		if (numberFor > 0) {
			generateLogicForModZeroRecursion(numberFor, numberFor / 3);
		} else {
			writer.write(seventhAxiom(THREE));
			reverseAB(mul(THREE, ZERO), ZERO);
		}
		
		writer.write(impl(equality(num(numberFor), mul(THREE, num(numberFor / 3))), res));
		writer.write(res.toString());
	}
	
	private void generateLogicForModZeroRecursion(final int big, final int small) {
		if (small == 0) {
			final Expression equal = seventhAxiom(THREE);
			writer.write(equal.toString());
			return;
		};
		
		writer.write(eightAxiom(3, small));
		writer.write(fifthAxiom(mul(3, small - 1), TWO));
		writer.write(firstAxiom(num(big - 1), sum(mul(3, small - 1), TWO)));
		writer.write(fifthAxiom(mul(3, small - 1), ONE).toString());
		writer.write(firstAxiom(num(big - 2), sum(mul(3, small - 1), ONE)));
		writer.write(fifthAxiom(mul(3, small - 1), ZERO));
		writer.write(firstAxiom(num(big - 3), sum(mul(3, small - 1), ZERO)));
		writer.write(sixAxiom(mul(3, small - 1)));
		
		generateLogicForModZeroRecursion(big - 3, (big - 3) / 3);
		
		reverseAB(sum(mul(3, small - 1), ZERO), mul(3, small -1));
		generateSecondAxiom(mul(3, small -1), num(big - 3), sum(mul(3, small - 1), ZERO));
		reverseAB(num(big - 2), inc(sum(mul(3, small - 1), ZERO)));
		reverseAB(sum(mul(3,small - 1), ONE), inc(sum(mul(3, small - 1), ZERO)));
		generateSecondAxiom(inc(sum(mul(3, small - 1), ZERO)), num(big - 2), sum(mul(3,small - 1), ONE));
		reverseAB(num(big - 1), inc(sum(mul(3,small - 1), ONE)));
		reverseAB(sum(mul(3, small - 1), TWO), inc(sum(mul(3,small - 1), ONE)));
		generateSecondAxiom(inc(sum(mul(3,small - 1), ONE)), num(big - 1), sum(mul(3, small - 1), TWO));
		reverseAB(num(big), inc(sum(mul(3, small - 1), TWO)));
		reverseAB(sum(mul(3, small - 1), THREE), inc(sum(mul(3, small - 1), TWO)));
		generateSecondAxiom(inc(sum(mul(3, small - 1), TWO)), num(big), sum(mul(3, small - 1), THREE));
		reverseAB(num(big), sum(mul(3, small - 1), THREE));
		reverseAB(mul(3, small), sum(mul(3, small - 1), THREE));
		generateSecondAxiom(sum(mul(3, small - 1), THREE), num(big), mul(3, small));
		reverseAB(num(big), mul(3, small));
	}
}
