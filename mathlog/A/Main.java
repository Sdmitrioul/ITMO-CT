import a.skroba.utils.parser.ExpressionParser;

import java.util.Scanner;

public class Main {
	public static void main(String[] args) {
		try (final Scanner scanner = new Scanner(System.in)) {
			if (scanner.hasNextLine()) {
				System.out.println(ExpressionParser.parse(scanner.nextLine()).toSimpleTreeGrammar());
			}
		}
	}
}
