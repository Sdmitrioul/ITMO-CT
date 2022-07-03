import d.skroba.Checker;
import d.skroba.ProofGenerator;
import d.skroba.util.Reader;
import d.skroba.util.Writer;

import java.util.Scanner;

public class Main {
	public static void main(String[] args) {
		try(final ProofGenerator generator = new ProofGenerator(new Writer("output"))) {
			final Scanner scanner = new Scanner(System.in);
			if (scanner.hasNextInt()) {
				generator.generate(scanner.nextInt());
			}
			scanner.close();
		}
	}
	
}
