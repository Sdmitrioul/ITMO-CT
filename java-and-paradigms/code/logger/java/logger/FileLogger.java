package logger;

import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;

public class FileLogger implements Logger {
    public Level level;
    private String loggerName;
    public boolean timeFormat;
    private BufferedWriter writer;
    private Date date;
    private final SimpleDateFormat formatForDateNow = new SimpleDateFormat("yyyy.MM.dd ', ' hh:mm:ss a ");

    public FileLogger(String loggerName) {
        this.loggerName = loggerName;
        timeFormat = false;
        date = new Date();
        level = Level.DEBUG;
        try {
            writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(loggerName + "-logFile.txt"), "utf8"));
        } catch (IOException e) {
            System.err.println("I/O error" + e.getMessage());
        }
    }

    public void log(Level level, String message) {
        try {
            try {
                if (level.getLevel() >= this.level.getLevel()) {
                    writer.write(level + ": " + message);
                    if (timeFormat) {
                        writer.newLine();
                        writer.write(formatForDateNow.format(date) + loggerName);
                    }
                }
            } finally {
                writer.close();
            }
        } catch (IOException e) {
            System.err.println("I/O error" + e.getMessage());
        }
    }

    public void log(Level level, String message, Throwable cause) {
        try {
            try {
                writer.write(level + ": " + message + " " + cause.getStackTrace());
                if (timeFormat) {
                    writer.newLine();
                    writer.write(formatForDateNow.format(date) + loggerName);
                }
            } finally {
                writer.close();
            }
        } catch (IOException e) {
            System.err.println("I/O error" + e.getMessage());
        }
    }

    @Override
    public void log(String message) {
        try {
            try {
                writer.write(message);
                if (timeFormat) {
                    writer.newLine();
                    writer.write(formatForDateNow.format(date) + loggerName);
                }
            } finally {
                writer.close();
            }
        } catch (IOException e) {
            System.err.println("I/O error" + e.getMessage());
        }
    }

    @Override
    public void log(String message, Throwable cause) {
        try {
            try {
                writer.write(message + " " + cause.getStackTrace());
                if (timeFormat) {
                    writer.newLine();
                    writer.write(formatForDateNow.format(date) + loggerName);
                }
            } finally {
                writer.close();
            }
        } catch (IOException e) {
            System.err.println("I/O error" + e.getMessage());
        }
    }
}
