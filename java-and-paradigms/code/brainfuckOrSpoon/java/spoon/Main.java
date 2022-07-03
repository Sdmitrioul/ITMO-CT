package spoon;

import spoon.token.Tokenizer;

import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        //String string = "11111111110010001011111110101111111111010111010101101101101100000110101100101001010010101111111001010001010111001010010110010100110111111111111111110010100100010101110010100000000000000000000010100000000000000000000000000010100101001010010001010";
        /*String string = "";
        for (int i = 0; i < args.length; i++) {
           string = args[0];
        }
        SpoonToBF spoonToBF = new SpoonToBF(string);
        int[] input = new int[spoonToBF.getCountOfInput()];
        Scanner scanner = new Scanner(System.in);
        for (int i = 0; i < spoonToBF.getCountOfInput(); i++) {
            boolean proverka = false;
            while (!proverka) {
                if (scanner.hasNextInt()) {
                    input[i] = scanner.nextInt();
                    proverka = true;
                } else if (scanner.hasNext()) {
                    scanner.next();
                }
            }
        }*/
        //SpoonToBF spoonToBF = new SpoonToBF(string);
        //Interpritator interpritator = new Interpritator(spoonToBF.getCommandOnBF(), inputin);
        StringBuilder string = new StringBuilder();
        int index = 1;
        int[] input = new int[10];
        while (index < args.length && !args[index].equals("input")) {
            string.append(args[index++]);
        }
        try {
            if (input.length - index > 10) {
                throw new TooMuchMemoryException();
            }
        } catch (TooMuchMemoryException e) {
            System.out.println("Too much input information: " + e.getMessage());
        }
        index++;
        int i = 0;
        while (index < args.length) {
            input[i++] = Integer.parseInt(args[index++]);
        }
        //String string = "00101100010100010110001010001011000101000101100010100010110001010001011000101000101100010100010110001010";
        //int[] inputin = {73, 84, 32, 87, 79, 82, 75, 83};
        Tokenizer tokenizer = new Tokenizer();
        tokenizer.tokenize(string.toString());
        Interpreter interpritator = new Interpreter(input, tokenizer);
        System.out.println(interpritator.getReturningString());
    }
}
