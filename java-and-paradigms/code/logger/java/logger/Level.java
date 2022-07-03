package logger;

public enum Level {
    DEBUG(0, "DEBUG"), INFO(1, "INFO"), WARNING(2, "WARNING"), ERROR(3, "ERROR");
    private int level;
    private String nameLevel;
    Level(int level, String nameLevel){
        this.level = level;
        this.nameLevel = nameLevel;
    }

    public int getLevel() {
        return level;
    }

    @Override
    public String toString() {
        return nameLevel;
    }
}
