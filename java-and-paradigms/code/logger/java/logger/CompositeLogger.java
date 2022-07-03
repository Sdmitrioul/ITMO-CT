package logger;

import java.io.*;
import java.text.SimpleDateFormat;
import java.util.Date;

public class CompositeLogger implements Logger {
    public Level level;
    private String loggerName;
    public boolean timeFormat;
    private final PrintStream printStream;
    private BufferedWriter writer;
    private Date date;
    private final SimpleDateFormat formatForDateNow = new SimpleDateFormat("yyyy.MM.dd ', ' hh:mm:ss a ");
    private  FileLogger logger1;
    private CompositeLogger logger2;

    public CompositeLogger(String loggerName) {
        this.loggerName = loggerName;
        printStream = System.out;
        date = new Date();
        level = Level.DEBUG;
        try {
            writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(loggerName + "-logFile.txt"), "utf8"));
        } catch (IOException e) {
            System.err.println("I/O error" + e.getMessage());
        }
        logger1 = new FileLogger(loggerName);
        logger2 = new CompositeLogger(loggerName);
    }

    public void log(Level level, String message) {
        syncronize();
        logger1.log(level, message);
        logger2.log(level, message);
    }

    public void log(Level level, String message, Throwable cause) {
        syncronize();
        logger1.log(level, message, cause);
        logger2.log(level, message, cause);
    }

    @Override
    public void log(String message) {
        syncronize();
        logger1.log(message);
        logger2.log(message);
    }

    @Override
    public void log(String message, Throwable cause) {
        syncronize();
        logger1.log(message, cause);
        logger2.log(message, cause);
    }

    private void syncronize() {
        logger1.level = this.level;
        logger2.level = this.level;
        logger1.timeFormat = timeFormat;
        logger2.timeFormat = timeFormat;
    }
}
