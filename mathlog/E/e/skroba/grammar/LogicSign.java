package e.skroba.grammar;

/**
 * Enum class for logic signs.
 */
public enum LogicSign {
	IMPLICATION("->"), DISJUNCTION("|"), CONJUNCTION("&"), NEGATION("!"), EQUAL("="), INCREMENT("'"), MUL("*"), SUM("+"), ALL("@"), EXIST("?");
	
	private final String sign;
	
	LogicSign(final String sign) {
		this.sign = sign;
	}
	
	@Override
	public String toString() {
		return this.sign;
	}
	
	public boolean equals(final LogicSign sign) {
		return sign.sign.equals(this.sign);
	}
}
