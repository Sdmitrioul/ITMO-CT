import java.io.*;
import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;

public class Main {
    public static void main(String[] args) {
        if (args.length == 1) {
            Sort sort = new Sort(readFromConsole());
            sort.sort();
            sort.print();
        } else {
            switch (args[1]) {
                case "-b" :
                    if (args.length == 2) {
                        IgnoreLeadingBlanks ignoreLeadingBlanks = new IgnoreLeadingBlanks(readFromConsole());
                        ignoreLeadingBlanks.sort();
                        ignoreLeadingBlanks.print();
                    } else {
                        for (int i = 2; i < args.length; i++) {
                            IgnoreLeadingBlanks ignoreLeadingBlanks = new IgnoreLeadingBlanks(readFromFile(args[i]));
                            ignoreLeadingBlanks.sort();
                            writeInFile(args[i], ignoreLeadingBlanks.printInFile());
                        }
                    }
                    break;
                case "-d" :
                    if (args.length == 2) {
                        DictionaryOrder dictionaryOrder = new DictionaryOrder(readFromConsole());
                        dictionaryOrder.sort();
                        dictionaryOrder.print();
                    } else {
                        for (int i = 2; i < args.length; i++) {
                            DictionaryOrder dictionaryOrder = new DictionaryOrder(readFromFile(args[i]));
                            dictionaryOrder.sort();
                            writeInFile(args[i], dictionaryOrder.printInFile());
                        }
                    }
                    break;
                case "-f" :
                    if (args.length == 2) {
                        IgnoreCase ignoreCase = new IgnoreCase(readFromConsole());
                        ignoreCase.sort();
                        ignoreCase.print();
                    } else {
                        for (int i = 2; i < args.length; i++) {
                            IgnoreCase ignoreCase = new IgnoreCase(readFromFile(args[i]));
                            ignoreCase.sort();
                            writeInFile(args[i], ignoreCase.printInFile());
                        }
                    }
                    break;
                case "i" :
                    if (args.length == 2) {
                        IgnoreNonprinting ignoreNonprinting = new IgnoreNonprinting(readFromConsole());
                        ignoreNonprinting.sort();
                        ignoreNonprinting.print();
                    } else {
                        for (int i = 2; i < args.length; i++) {
                            IgnoreNonprinting ignoreNonprinting = new IgnoreNonprinting(readFromFile(args[i]));
                            ignoreNonprinting.sort();
                            writeInFile(args[i], ignoreNonprinting.printInFile());
                        }
                    }
                    break;
                case "-n" :
                    if (args.length == 2) {
                        Numeric numeric = new Numeric(readFromConsole());
                        numeric.sort();
                        numeric.print();
                    } else {
                        for (int i = 2; i < args.length; i++) {
                            Numeric numeric = new Numeric(readFromFile(args[i]));
                            numeric.sort();
                            writeInFile(args[i], numeric.printInFile());
                        }
                    }
                    break;
                case "-r" :
                    if (args.length == 2) {
                        Reverse reverse = new Reverse(readFromConsole());
                        reverse.sort();
                        reverse.print();
                    } else {
                        for (int i = 2; i < args.length; i++) {
                            Reverse reverse = new Reverse(readFromFile(args[i]));
                            reverse.sort();
                            writeInFile(args[i], reverse.printInFile());
                        }
                    }
                    break;
                default:
                    for (int i = 1; i < args.length; i++) {
                        Sort sort = new Sort(readFromFile(args[i]));
                        sort.sort();
                        writeInFile(args[i], sort.printInFile());
                    }
                    break;
            }
        }
    }

    private static ArrayList<String> readFromFile(String filename) {
        ArrayList<String> list = new ArrayList<>();
        try {
            Scanner scanner = new Scanner(filename);
            try {
                while (scanner.hasLine()) {
                    String string = scanner.getLine();
                    if (string.equals("")) {
                        break;
                    }
                    list.add(string);
                }
            } finally {
                scanner.close();
            }
        } catch (UnsupportedEncodingException e) {
            System.out.println("Encoding problem : " + e.getMessage());
        } catch (IOException e) {
            System.out.println("Some problem : " + e.getMessage());
        }
        return list;
    }

    private static ArrayList<String> readFromConsole() {
        ArrayList<String> list = new ArrayList<>();
        try {
            Scanner scanner = new Scanner(System.in);
            try {
                while (scanner.hasLine()) {
                    String string = scanner.getLine();
                    if (string.equals("")) {
                        break;
                    }
                    list.add(string);
                }
            } finally {
                scanner.close();
            }
        } catch (UnsupportedEncodingException e) {
            System.out.println("Encoding problem : " + e.getMessage());
        } catch (IOException e) {
            System.out.println("Some problem : " + e.getMessage());
        }
        return list;
    }

    private static void writeInFile(String filename, ArrayList<String> list) {
        try {
            BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("Sorted" + filename), "utf8"));
            try {
                for (String iterator : list) {
                    writer.write(iterator);
                    writer.newLine();
                }
            } finally {
                writer.close();
            }
        } catch (IOException e) {
            System.err.println("I/O error" + e.getMessage());
        }
    }
}
