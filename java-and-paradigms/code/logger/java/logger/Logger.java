package logger;

public interface Logger {
    public Level level = Level.DEBUG;
    public boolean timeFormat = false;
    public void log(String message);
    public void log(String message, Throwable cause);
    public void log(Level level, String message);
    public void log(Level level, String message, Throwable cause);
}
