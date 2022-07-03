package logger;

public class Main {
    public static void main(String[] args) {
        ConsoleLogger logger = new ConsoleLogger(Main.class.getName());
        logger.timeFormat = true;
        //logger.level = Level.INFO;
        logger.log(Level.INFO, "Начало выполненя программы");
        try {
            for (int i = 5; i >= 0; i--) {
                double result = 5 / i;
                logger.log(Level.DEBUG, "Частное числа 5 и "  + i + " равно - " + result);
            }
        } catch (ArithmeticException e) {
            logger.log(Level.ERROR, "Неправильный формат числа :", e);
        } finally {
            logger.log(Level.WARNING, "Принудительное завершение программы");
        }
        logger.log("Конец программы");
    }
}
