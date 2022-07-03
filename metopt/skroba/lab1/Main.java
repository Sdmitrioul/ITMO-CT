package skroba.lab1;

import skroba.lab1.methods.*;
import skroba.exceptions.TimeOutException;
import skroba.lab1.utils.BufferedAnswerWriter;
import skroba.lab1.utils.data.Answer;

import java.io.BufferedWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;

public class Main {
    private final static Path root = Paths.get(System.getProperty("user.dir"))
            .resolve("src").resolve("skroba")
            .resolve("lab1").resolve("results");
    private final static double EPS = 0.0001;
    private final static double DELTA = 1e-5;
    private final static Function<Double, Double> FUN = (x) -> Math.pow(Math.E, 3 * x) + 5 * Math.pow(Math.E, -2 * x);
    private final static Function<Double, Double> MULTI_MODAL_FUN = (x) -> (x-1)*(x-3)*(x-5)*(x-7)*(x-9);
    private final static double LEFT = 0;
    private final static double RIGHT = 1;
    private final static List<Class<?>> METHODS = List.of(DichotomyMethod.class, GoldenRatioMethod.class, FibonacciMethod.class, ParabolaMethod.class, BrentCombineMethod.class);
    private final static List<Double> EPSIS = List.of(0.05, 0.01, 0.005, 0.001, 5.0E-4, 1.0E-4, 5.0E-5, 1.0E-5, 5.0E-6, 1.0E-6, 5.0E-7, 1.0E-7, 5.0E-8, 1.0E-8, 5.0E-9, 1.0E-9);
    
    public static void main(String[] args) {
        Answer[] answers = new Answer[5];
        MinimumSearcher[] searchers = new MinimumSearcher[5];
        putSearchers(searchers);
        putAnswers(answers, searchers);
        Arrays.stream(answers).forEach(Main::write);
        findCountDependency();
        //multiModalTest();
    }
    
    private static void multiModalTest() {
        try (BufferedWriter writer = Files.newBufferedWriter(root.resolve("multiModalTest.txt")))  {
            double left = 1;
            double right = 10;
            for (Class<?> token : METHODS) {
                MinimumSearcher searcher = (MinimumSearcher) token.getDeclaredConstructors()[0].newInstance(EPS, DELTA, MULTI_MODAL_FUN);
                Answer ans = searcher.findMin(left, right);
                writer.write(ans.getMethodName() + " & ");
                writer.write(String.format("%.5f \\\\\\hline\n", ans.getMin()));
            }
        } catch (IOException e) {
            System.err.println("Dependency writer problem: " + e.getMessage());
        } catch (IllegalAccessException | InstantiationException | InvocationTargetException e) {
            System.err.println("Can't create class");
        }
    }
    
    private static void findCountDependency() {
        try (BufferedWriter writer = Files.newBufferedWriter(root.resolve("dependencyResult.txt")))  {
            for (Double eps : EPSIS) {
                writeOutput(writer, eps);
            }
        } catch (IOException e) {
            System.err.println("Dependency writer problem: " + e.getMessage());
        } catch (IllegalAccessException | InstantiationException | InvocationTargetException e) {
            System.err.println("Can't create class");
        }
    }
    
    private static void writeOutput(final BufferedWriter writer, final double eps) throws IOException, IllegalAccessException, InvocationTargetException, InstantiationException {
        writer.write(eps + " ");
        for (Class<?> token : METHODS) {
            MinimumSearcher searcher = (MinimumSearcher) token.getDeclaredConstructors()[0].newInstance(eps, DELTA, FUN);
            Answer ans = searcher.findMin(LEFT, RIGHT);
            writer.write(ans.getOperationsCounter() + " ");
        }
        writer.newLine();
    }
    
    private static void putAnswers(Answer[] answers, MinimumSearcher[] searchers) {
        for (int i = 0; i < searchers.length; i++) {
            answers[i] = getAnswer(searchers[i]);
        }
    }
    
    private static void putSearchers(MinimumSearcher[] searchers) {
        searchers[0] = new DichotomyMethod(EPS, DELTA, FUN);
        searchers[1] = new FibonacciMethod(EPS, DELTA, FUN);
        searchers[2] = new GoldenRatioMethod(EPS, DELTA, FUN);
        searchers[3] = new ParabolaMethod(EPS, DELTA, FUN);
        searchers[4] = new BrentCombineMethod(EPS, DELTA, FUN);
    }
    
    private static Answer getAnswer(MinimumSearcher searcher) {
        Answer ans = null;
        try {
            ans = searcher.findMin(LEFT, RIGHT);
        } catch (TimeOutException e) {
            System.err.println(e.getMessage());
            ans = searcher.answerWithException();
        }
        return ans;
    }
    
    private static void write(Answer answer) {
        try (BufferedAnswerWriter writer = new BufferedAnswerWriter(root, answer)) {
        } catch (IOException e) {
            System.err.println("Problem with writing in file");
        }
    }
}
