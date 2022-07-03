package info.kgeorgiy.ja.skroba.walk;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;

public final class WriterPJWHash {
    private final static int BUFFER_SIZE = 1024;
    static long hash(final Path inputPath) {
        byte[] buffer = new byte[BUFFER_SIZE];
        int pointer = -1;
        long hash = 0;
        try (final InputStream reader = Files.newInputStream(inputPath)) {
            while ((pointer = reader.read(buffer)) >= 0) {
                for (int i = 0; i < pointer; i++) {
                    hash = (hash << 8) + (buffer[i] & 0xff);
                    final long high = hash & 0xff00_0000_0000_0000L;
                    if (high != 0) {
                        hash ^= high >> 48;
                        hash &= ~high;
                    }
                }
            }
        } catch (IOException | InvalidPathException e) {
            System.err.println("Problems with open or reading file for hash calculating: " + e.getMessage());
        }
        return hash;
    }
}
