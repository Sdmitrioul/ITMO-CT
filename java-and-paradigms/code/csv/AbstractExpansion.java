package csv;

import java.io.*;
import java.util.*;

public abstract class AbstractExpansion {
    private Map<String, Map<String, Integer>> list;
    private List<String> columnName;
    private Character delimiter;
    private Character quote;

    public void treatment(char delimiter, char quote, String filename) {
        this.delimiter = delimiter;
        this.quote = quote;
        list = new LinkedHashMap<>();
        columnName = new ArrayList<>();
        try {
            Scanner scanner = new Scanner(filename);
            try {
                if (delimiter != '\t') {
                    String string = scanner.getLine();
                    readHeadings(string);
                    while (scanner.hasLine()) {
                        string = scanner.getLine();
                        readOnLine(string);
                    }
                    for (Map.Entry<String, Map<String, Integer>> entry : list.entrySet()) {
                        writingInFile(entry.getKey(), filename.substring(filename.length() - 4), entry.getValue());
                    }
                } else {
                    String string = scanner.getLine();
                    readTSV(string, true);
                    while (scanner.hasLine()) {
                        string = scanner.getLine();
                        readTSV(string, false);
                    }
                    for (Map.Entry<String, Map<String, Integer>> entry : list.entrySet()) {
                        writingInFile(entry.getKey(), filename.substring(filename.length() - 4), entry.getValue());
                    }
                }
            } finally {
                scanner.close();
            }
        } catch (FileNotFoundException e) {
            System.err.println("Input File not found " + e.getMessage());
            return;
        } catch (IOException e) {
            System.err.println("Input error: " + e.getMessage());
            return;
        }
    }

    private void readOnLine(String string) {
        int Index = 0;
        for (int i = 0; i < list.size(); i++) {
            while (Index < string.length() && string.charAt(Index) != delimiter) {
                list.get(columnName.get(i)).put(readDelimiters(Index, string), list.get(columnName.get(i)).getOrDefault(readDelimiters(Index, string), 0) + 1);
                System.out.println(readDelimiters(Index, string));
                Index += readDelimiters(Index, string).length();
                if (Index < string.length() && string.charAt(Index) == delimiter) {
                    Index++;
                    break;
                }
            }
        }
        while (Index < string.length()) {
            System.out.println(readDelimiters(Index, string));
            list.get(columnName.get(columnName.size() - 1)).put(readDelimiters(Index, string), list.get(columnName.get(columnName.size() - 1)).getOrDefault(readDelimiters(Index, string), 0) + 1);
            Index += readDelimiters(Index, string).length();
        }
    }

    private void readHeadings(String string) {
        int Index = 0;
        int prevIndex = 0;
        while (Index < string.length()) {
            prevIndex++;
            if (prevIndex < string.length() && string.charAt(prevIndex) == delimiter) {
                prevIndex++;
            } else {
                Index = readHeader(prevIndex, string);
                String word = string.substring(prevIndex, Index);
                System.out.println(string.substring(prevIndex, Index));
                list.put(word, new LinkedHashMap<>());
                columnName.add(word);
                prevIndex = Index++;
            }
        }
    }

    private String readDelimiters(int beginIndex, String string) {
        int endIndex = beginIndex + 1;
        while (endIndex < string.length() && string.charAt(endIndex) != quote) {
            endIndex++;
        }
        endIndex++;
        return string.substring(beginIndex, endIndex);
    }

    private int readHeader(int beginIndex, String string) {
        int retIndex = beginIndex;
        while (retIndex < string.length() && string.charAt(retIndex) != quote) {
            retIndex++;
        }
        return retIndex++;
    }

    private void writingInFile(String string, String fileformat, Map<String, Integer> map) {
        try {
            BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(string + fileformat), "utf8"));
            try {
                List<Map.Entry<String, Integer>> entries = new ArrayList<>(map.entrySet());
                Collections.sort(entries, new Comparator<Map.Entry<String, Integer>>() {
                    @Override
                    public int compare(Map.Entry<String, Integer> o1, Map.Entry<String, Integer> o2) {
                        return o1.getValue().compareTo(o2.getValue());
                    }
                });

                System.out.println(string);
                if (!fileformat.equals(".tsv")) {
                    writer.write(quote + "Value" + quote + delimiter + quote + "Frequency" + quote);
                    writer.newLine();
                } else {
                    writer.write("Value" + delimiter + "Frequency");
                    writer.newLine();
                }
                if (!fileformat.equals(".tsv")) {
                    for (int i = entries.size() - 1; i >= 0; i--) {
                        writer.write(entries.get(i).getKey() + delimiter + quote + entries.get(i).getValue() + quote);
                        writer.newLine();
                    }
                } else {
                    for (int i = entries.size() - 1; i >= 0; i--) {
                        writer.write(entries.get(i).getKey() + delimiter + entries.get(i).getValue());
                        writer.newLine();
                    }
                }
            } finally {
                writer.close();
            }
        } catch (IOException e) {
            System.err.println("I/O error" + e.getMessage());
        }
    }

    private void readTSV(String string, boolean headingIf) {
        if (headingIf) {
            int beginIndex = 0;
            int endIndex = 0;
            while (endIndex < string.length()) {
                endIndex++;
                if (endIndex == string.length() || string.charAt(endIndex) == ' ') {
                    if (string.charAt(beginIndex) == ' ') {
                        beginIndex++;
                    }
                    String word = string.substring(beginIndex, endIndex);
                    list.put(word, new LinkedHashMap<>());
                    columnName.add(word);
                    System.out.println(word);
                    beginIndex = endIndex++;
                }
            }
        } else {
            int beginIndex = 0;
            int endIndex = 0;
            int i = 0;
            while (endIndex < string.length()) {
                endIndex++;
                if (endIndex == string.length() || string.charAt(endIndex) == ' ') {
                    if (string.charAt(beginIndex) == ' ') {
                        beginIndex++;
                    }
                    String word = string.substring(beginIndex, endIndex);
                    list.get(columnName.get(i)).put(word, list.get(columnName.get(i)).getOrDefault(word, 0) + 1);
                    System.out.println(word);
                    beginIndex = endIndex++;
                    i++;
                }
            }
        }
    }
}
