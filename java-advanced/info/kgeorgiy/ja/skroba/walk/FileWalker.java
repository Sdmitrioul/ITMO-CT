package info.kgeorgiy.ja.skroba.walk;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;

public abstract class FileWalker {
    protected static boolean validArguments(String[] args) {
        return args != null && args.length == 2 && args[0] != null && args[1] != null;
    }

    protected void process(final String inputFilename, final String outputFilename) {
        try {
            Path inputPath = Paths.get(inputFilename);
            try {
                Path outputPath = Paths.get(outputFilename);
                try {
                    if (outputPath.getParent() != null && !Files.exists(outputPath.getParent())) {
                        Files.createDirectories(outputPath.getParent());
                    }
                    openFiles(inputPath, outputPath);
                } catch (IOException e) {
                    System.err.println("Can't create output file directory: " + e.getMessage());
                }
            } catch (InvalidPathException e) {
                System.err.println("Problems with path to output file: " + e.getMessage());
            }
        } catch (InvalidPathException e) {
            System.err.println("Problems with path to input file: " + e.getMessage());
        }
    }

    private void openFiles(final Path inputPath, final Path outputPath) {
        try (final BufferedReader inputReader = Files.newBufferedReader(inputPath, StandardCharsets.UTF_8)) {
            try (final BufferedWriter outputWriter = Files.newBufferedWriter(outputPath, StandardCharsets.UTF_8)) {
                walk(inputReader, outputWriter);
            } catch (IOException e) {
                System.err.println("Problems with open output writer file: " + e.getMessage());
            }
        } catch (IOException e) {
            System.err.println("Problems with open input reader file: " + e.getMessage());
        }
    }

    private void walk(final BufferedReader inputReader, final BufferedWriter outputWriter) {
        try {
            String filename = inputReader.readLine();
            while (filename != null) {
                try {
                    final Path path = Paths.get(filename);
                    pathOperating(path, outputWriter);
                } catch (InvalidPathException e) {
                    System.err.println("Problems with path to file: " + e.getMessage());
                    outputWriter.write("0000000000000000" + " " + filename + "\n");
                }
                filename = inputReader.readLine();
            }
        } catch (IOException e) {
            System.err.println("Reading input file problem or writing output file problem: " + e.getMessage());
        }
    }

    protected abstract void pathOperating(final Path path, final BufferedWriter outputWriter) throws IOException;
}
