package grep;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class Grep {
    private static boolean ignoreCase = false;
    private static List<String> list;

    public static void main(String[] args) {
        if (args[1].equals("-i")) {
            ignoreCase = true;
            words(args[2]);
            if (args.length > 3) {
                for (int i = 3; i < args.length; i++) {
                    grepFromFile(args[i]);
                }
            } else {
                grepFromConsole();
            }
        } else {
            words(args[1]);
            if (args.length > 2) {
                for (int i = 2; i < args.length; i++) {
                    grepFromFile(args[i]);
                }
            } else {
                grepFromConsole();
            }
        }
        /*String string = "привет|пока|да|45|6";
        words(string);
        for (String out : list) {
            System.out.println(out);
        }*/
    }

    private static void grepFromFile(String filename) {
        try {
            Reader reader = new InputStreamReader(new FileInputStream(filename), "utf8");
            File output = new File("TMP" + filename);
            Writer writer = new FileWriter(output);
            char[] sym = new char[256];
            int vvod = reader.read(sym);
            int index = 0;
            while (vvod != -1) {
                if (index == vvod) {
                    vvod = reader.read(sym);
                    index = 0;
                } else {
                    if (sym[index] == '\n') {
                        writer.close();
                        checkLine("TMP" + filename);
                        output.delete();
                        writer = new FileWriter(output);
                    } else {
                        writer.write(sym[index]);
                    }
                    index++;
                }
            }
            writer.close();
            checkLine("TMP" + filename);
            output.delete();
        } catch (FileNotFoundException e) {
            System.out.println("File not found: " + e.getMessage());
        } catch (IOException e) {
            System.out.println("Something goes wrong: " + e.getMessage());
        }
    }

    private static void grepFromConsole() {
        try {
            Reader reader = new InputStreamReader(System.in, "utf8");
            File output = new File("TMP");
            Writer writer = new FileWriter(output);
            char[] sym = new char[256];
            int vvod = reader.read(sym);
            int index = 0;
            while (vvod != -1) {
                if (index == vvod) {
                    vvod = reader.read(sym);
                    index = 0;
                } else {
                    if (sym[index] == '\n') {
                        writer.close();
                        checkLine("TMP");
                        output.delete();
                        writer = new FileWriter(output);
                    } else {
                        writer.write(sym[index]);
                    }
                    index++;
                }
            }
            writer.close();
            checkLine("TMP");
            output.delete();
        } catch (IOException e) {
            System.out.println("Something goes wrong: " + e.getMessage());
        }
    }

    private static void checkLine(String filename) {
        try {
            Reader reader = new InputStreamReader(new FileInputStream(filename), "utf8");
            char[] chars = new char[256];
            char[] next = new char[1];
            int vvod = reader.read(chars);
            StringBuilder sb = new StringBuilder();
            sb.append(chars.toString());
            while (vvod != -1) {
                boolean proverka = false;
                for (String word : list) {
                    if (ignoreCase) {
                        if (sb.toString().toLowerCase().contains(word)) {
                            writeFromFile(filename);
                            proverka = true;
                            reader.close();
                            break;
                        }
                    } else {
                        if (sb.toString().contains(word)) {
                            writeFromFile(filename);
                            proverka = true;
                            reader.close();
                            break;
                        }
                    }
                }
                if (proverka) {
                    break;
                } else {
                    vvod = reader.read(next);
                    if (vvod == -1) {
                        break;
                    } else {
                        sb.deleteCharAt(0).append(next);
                    }
                }
            }
            reader.close();
        } catch (FileNotFoundException e) {
            System.out.println("File not found: " + e.getMessage());
        } catch (IOException e) {
            System.out.println("Something goes wrong: " + e.getMessage());
        }
    }

    private static void writeFromFile(String filename) {
        try {
            Reader reader = new InputStreamReader(new FileInputStream(filename), "utf8");
            char[] chars = new char[256];
            int vvod = reader.read(chars);
            while (vvod != -1) {
                for (int i = 0; i < vvod; i++)
                    System.out.print(chars[i]);
                vvod = reader.read(chars);
            }
            System.out.println();
            reader.close();
        } catch (FileNotFoundException e) {
            System.out.println("File not found: " + e.getMessage());
        } catch (IOException e) {
            System.out.println("Something goes wrong: " + e.getMessage());
        }
    }

    private static void words(String string) {
        list = new ArrayList<>();
        int beginIndex = 0;
        int endIndex = 0;
        while (endIndex < string.length()) {
            endIndex++;
            if (endIndex == string.length()) {
                if (ignoreCase)
                    list.add(string.substring(beginIndex, endIndex).toLowerCase());
                else
                    list.add(string.substring(beginIndex, endIndex));
            } else if (string.charAt(endIndex) == '|') {
                if (ignoreCase)
                    list.add(string.substring(beginIndex, endIndex).toLowerCase());
                else
                    list.add(string.substring(beginIndex, endIndex));
                beginIndex = endIndex + 1;
                endIndex++;
            }
        }
    }
}
