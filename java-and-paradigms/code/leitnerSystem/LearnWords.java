package leitnerSystem;

import java.io.IOException;

public class LearnWords {
    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(System.in);
        while (true) {
            if (scanner.hasLine()) {
                String string = scanner.getLine();
                System.out.println(string);
                if (string.equals("") || string == null) {
                    System.out.println("this is end");
                    break;
                }
            }
        }
    }
}
