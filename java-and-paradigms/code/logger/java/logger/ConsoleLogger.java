package logger;

import java.io.PrintStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Scanner;

public class ConsoleLogger implements Logger {
    public Level level;
    private String loggerName;
    public boolean timeFormat;
    private final PrintStream printStream;
    private Date date;
    private final SimpleDateFormat formatForDateNow = new SimpleDateFormat("yyyy.MM.dd ', ' hh:mm:ss a ");

    public ConsoleLogger(String loggerName) {
        this.loggerName = loggerName;
        printStream = System.out;
        date = new Date();
        level = Level.DEBUG;
        timeFormat = false;
    }

    public void log(Level level, String message) {
        if (level.getLevel() >= this.level.getLevel()) {
            printStream.println(level + ": " + message);
            if (timeFormat) {
                printStream.println(formatForDateNow.format(date) + loggerName);
            }
        }
    }

    public void log(Level level, String message, Throwable cause) {
        printStream.println(level + ": " + message + " " + cause.getStackTrace());
        if (timeFormat) {
            printStream.println(formatForDateNow.format(date) + loggerName);
        }
    }

    @Override
    public void log(String message) {
        printStream.println(message);
        if (timeFormat) {
            printStream.println(formatForDateNow.format(date) + loggerName);
        }
    }

    @Override
    public void log(String message, Throwable cause) {
        printStream.println(message + " " + cause.getStackTrace());
        if (timeFormat) {
            printStream.println(formatForDateNow.format(date) + loggerName);
        }
    }
}
