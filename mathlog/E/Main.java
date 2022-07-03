import e.skroba.generator.Generator;
import e.skroba.generator.GeneratorForThreeImpl;
import e.skroba.util.Writer;

import java.util.Scanner;

public class Main {
	public static void main(String[] args) throws Exception {
		try(final Generator generator = new GeneratorForThreeImpl(new Writer())) {
			final Scanner scanner = new Scanner(System.in);
			if (scanner.hasNextInt()) {
				generator.generateProofing(scanner.nextInt());
			}
			scanner.close();
		}
	}
	
}
