package info.kgeorgiy.ja.skroba.walk;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.*;

public class RecursiveWalk extends FileWalker {
    public static void main(String[] args) {
        if (validArguments(args)) {
            RecursiveWalk walker = new RecursiveWalk();
            walker.process(args[0], args[1]);
        } else {
            System.err.println("Format of launch: java RecursiveWalk <input filename> <output filename>");
        }
    }

    protected void pathOperating(final Path path, final BufferedWriter outputWriter) throws IOException {
        if (Files.isDirectory(path)) {
            try (DirectoryStream<Path> stream = Files.newDirectoryStream(path)) {
                for (Path entry: stream) {
                    if (!Files.isSameFile(path, entry)) {
                        pathOperating(entry, outputWriter);
                    }
                }
            }
        } else {
            final String hash = String.format("%016x", WriterPJWHash.hash(path));
            outputWriter.write(hash + " " + path.toString() + "\n");
        }
    }
}
