package leitnerSystem;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class LeitnerSystem {
    private final static String filename = "leitnerSystem.LeitnerSystem.txt";
    private LeitnerBox[] leitnerBoxes;
    private final static int[] randoms = new int[] {3, 12, 39, 120, 363, 1092, 3279, 9840, 29523, 88572};
    private final int maxRandom = 88573;

    public LeitnerSystem(String filename) {
        leitnerBoxes = new LeitnerBox[10];
        if (filename.equals("")) {
            readBoxes(this.filename);
        } else {
            readWords(filename);
        }
        learnWords();
    }

    private void readBoxes(String filename) {
        try {
            Scanner scanner = new Scanner(filename);
            try {
                for (int i = 0; i < 10; i++) {
                    String boxFileName = scanner.getLine();
                    leitnerBoxes[i] = new LeitnerBox(boxFileName);
                }
            } catch (IOException e) {
                System.out.println("Something goes wrong, file was damaged");
            } finally {
                scanner.close();
            }
        } catch (FileNotFoundException e) {
            System.out.println("There's miss information, file was deleted");
        } catch (UnsupportedEncodingException e) {
            System.out.println("There's encoding exception" + e.getMessage());
        } catch (IOException e) {
            System.out.println("Something goes wrong");
        }
    }

    private void readWords(String filename) {
        List<String> words = new ArrayList<>();
        int countWords = 0;
        try {
            Scanner scanner = new Scanner(filename);
            try {
                while (scanner.hasLine()) {
                    String string = scanner.getLine();
                    words.add(string);
                    countWords++;
                }
            } catch (IOException e) {
                System.out.println("Something goes wrong, file was damaged");
            } finally {
                scanner.close();
            }
        } catch (FileNotFoundException e) {
            System.out.println("There's miss information, file was deleted");
        } catch (UnsupportedEncodingException e) {
            System.out.println("There's encoding exception" + e.getMessage());
        } catch (IOException e) {
            System.out.println("Something goes wrong");
        }
        int index = 0;
        for (int i = 0; i < 10; i++) {
            leitnerBoxes[i] = new LeitnerBox();
            if (i != 9) {
                int tmp = countWords / 10;
                for (int j = 0; j < tmp; j++) {
                    leitnerBoxes[i].putWordIntoBox(words.get(index));
                    index++;
                }
            } else {
                for (int j = index; j < words.size(); j++) {
                    leitnerBoxes[i].putWordIntoBox(words.get(j));
                }
            }
        }
    }

    public void learnWords() {
        Random random = new Random();
        try {
            Scanner scanner = new Scanner(System.in);
            try {
                while (true) {
                    int randNumber = random.nextInt(maxRandom);
                    int boxNumber = 0;
                    String word = "Something goes wrong";
                    for (int i = 0; i < 10; i++) {
                        if (randNumber > randoms[i]) {
                            continue;
                        } else {
                            boxNumber = i;
                            if (!leitnerBoxes[i].isEmpty()) {
                                word = leitnerBoxes[i].getRandomWord();
                                System.out.println("Please, write translation of word - " + word);
                                String written = scanner.getLine();
                                if (!written.equals("")) {
                                    if (leitnerBoxes[boxNumber].checkAnswear(word, cleanTranslation(written))) {
                                        if (boxNumber < 9) {
                                            String tmp = leitnerBoxes[boxNumber].moveWordFromBox(word);
                                            leitnerBoxes[boxNumber + 1].putWordIntoBox(tmp);
                                        }
                                        System.out.println("You are right");
                                    } else {
                                        String tmp = leitnerBoxes[boxNumber].moveWordFromBox(word);
                                        leitnerBoxes[0].putWordIntoBox(tmp);
                                        System.out.println("You are wrong");
                                    }
                                } else {
                                    close();
                                }
                            } else {
                                learnWords();
                            }
                            break;
                        }
                    }
                }
            } catch (IOException e) {
                System.out.println("Reading problem");
            } finally {
                scanner.close();
            }
        } catch (UnsupportedEncodingException e) {
            System.out.println(e.getMessage());
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }

    private String cleanTranslation(String string) {
        int beginIndex = 0;
        while (Character.isWhitespace(string.charAt(beginIndex))) {
            beginIndex++;
        }
        int endindex = string.length() - 1;
        while (Character.isWhitespace(string.charAt(endindex))) {
            endindex--;
        }
        return string.substring(beginIndex, ++endindex);
    }

    private void close() {
        try {
            BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename), "utf8"));
            try {
                for (int i = 0; i < 10; i++) {
                    leitnerBoxes[i].close(i);
                    writer.write("LeitnerBox_" + i + ".txt");
                    writer.newLine();
                }
            } finally {
                writer.close();
                System.exit(0);
            }
        } catch (IOException e) {
            System.err.println("I/O error" + e.getMessage());
        }
    }
}
