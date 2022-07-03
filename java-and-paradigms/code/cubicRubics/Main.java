package cubicRubics;

import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        Cubi cubi = new Cubi();
        System.out.println("Commands look like (R`-reverse rotation of the right side, L - rotation of the left side)");
        System.out.println("If you want to stop, write: END");
        System.out.println(cubi.toString());
        Scanner scanner = new Scanner(System.in);
        while (true) {
            if (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                if (line.equals("END")) {
                    break;
                }
                Parser parser = new Parser(line);
                Command[] commands = parser.getCommands();
                for (Command command : commands) {
                    cubi.rotation(command);
                }
                System.out.println(cubi.toString());
            }
        }
        System.out.println("Last Position: ");
        System.out.println(cubi.toString());
    }
}
