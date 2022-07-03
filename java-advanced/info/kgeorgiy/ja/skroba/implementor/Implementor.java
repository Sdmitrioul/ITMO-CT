package info.kgeorgiy.ja.skroba.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.net.URISyntaxException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.jar.Attributes;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;

public class Implementor implements JarImpler {
    /** System dependency line separator.*/
    private final String LINE_SEPARATOR = System.lineSeparator();
    
    /** Empty string element.*/
    private final String EMPTY_STRING = "";
    
    /** String of one space.*/
    private final String SPACE = " ";
    
    /**
     * Function checking arguments that was given to main. Arguments must contain only one not null argument
     *
     * @param args - string array.
     * @return true - contain one not null argument, otherwise false
     */
    protected static boolean validArguments(final String[] args) {
        return args != null && args.length == 1 && args[0] != null;
    }
    
    /**
     * Main function for debug. Saving java .jar file, that you want to implement in the directory where java was run from.
     * Form of launch is java  -jar Implementor.jar full class name that you want to implement
     *
     * @param args args[0] - name of generate class.
     */
    public static void main(final String[] args) {
        if (validArguments(args)) {
            process(args[0]);
        } else {
            System.err.println("Format of launch: java -jar Implementor.jar <full class name>");
        }
    }
    
    /**
     * Helping function for main. It generate new instance of Implementor and invoke <strong>implement</strong> function with Class token from classname and dir where java was run from.
     *
     * @param className - name of generate class.
     */
    private static void process(final String className) {
        try {
            final Class<?> token = Class.forName(className);
            final Implementor implementor = new Implementor();
            implementor.implementJar(token, Paths.get(token.getSimpleName() + "Impl.jar"));
        } catch (ClassNotFoundException e) {
            System.err.println(className + " is unknown class name for implementing: " + e.getMessage());
        } catch (ImplerException e) {
            System.err.println("Can't implement class " + className + ", problem: " + e.getMessage());
        }
    }
    
    /**
     * Generate jar file with realization of java interface specified by provided <strong>token</strong>.
     *
     * It is located at <strong>root</strong>/<strong>token</strong> package.
     * Generated class named as <strong>token</strong> class name with suffix <strong>Impl</strong>.
     *
     * @param token type token to create implementation for.
     * @param jarFile is a directory contains it.
     * @throws info.kgeorgiy.java.advanced.implementor.ImplerException when implementation cannot be
     * generated or jar file can't be written.
     */
    @Override
    public void implementJar(final Class<?> token, final Path jarFile) throws ImplerException {
        checkArguments(token, jarFile);
        
        checkParentDir(jarFile);
        
        Path temporaryDir = null;
        try {
            temporaryDir = Files.createTempDirectory(jarFile.getParent(), "jarContainer");
            createJarFile(token, jarFile, temporaryDir);
        } catch (IOException e) {
            System.err.println("Can't create temp directory: " + e.getMessage());
        } catch (URISyntaxException e) {
            System.err.println("Can't get source files");
        } finally {
            deleteTempDir(temporaryDir);
        }
    }
    
    /**
     * Delete directory with all files inside.
     *
     * @param temporaryDir - deleting directory.
     */
    private void deleteTempDir(Path temporaryDir) {
        try {
            Files.walkFileTree(temporaryDir, new Implementor.DELETER());
        } catch (IOException e) {
            System.err.println("Can't delete temp directory: " + e.getMessage());
        }
    }
    
    /**
     * This function generate java class and compile it, then make from jar file.
     *
     * @param token type token to create implementation for.
     * @param jarFile is a directory contains it.
     * @param temporaryDir directory for temporary files.
     * @throws info.kgeorgiy.java.advanced.implementor.ImplerException when implementation cannot be
     * generated or jar file can't be written.
     * @throws URISyntaxException when can't get information of needed files.
     */
    private void createJarFile(final Class<?> token, final Path jarFile, final Path temporaryDir) throws ImplerException, URISyntaxException {
        implement(token, temporaryDir);
        Path compilingFile = temporaryDir.resolve(Paths.get(getPackageName(token))).resolve(token.getSimpleName() + "Impl.java");
        String[] arguments = new String[]{
                "-cp",
                Paths.get(token.getProtectionDomain().getCodeSource().getLocation().toURI()).toString(),
                compilingFile.toString()
        };
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        
        if (compiler == null || compiler.run(null, null, null, arguments) != 0) {
            throw new ImplerException("Problem with compiler");
        }
        
        writeJarFile(token, jarFile, temporaryDir);
    }
    
    /**
     * This function generate zip jar file
     *
     * @param token type token to create implementation for.
     * @param jarFile is a directory contains it.
     * @param temporaryDir directory for temporary files.
     * @throws info.kgeorgiy.java.advanced.implementor.ImplerException when jar file can't be written.
     */
    private void writeJarFile(final Class<?> token, final Path jarFile, final Path temporaryDir) throws ImplerException {
        Manifest manifest = new Manifest();
        manifest.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0");
        Path compiledFile = temporaryDir.resolve(getPackageName(token)).resolve(token.getSimpleName() + "Impl.class");
        try (JarOutputStream jarOutputStream = new JarOutputStream(Files.newOutputStream(jarFile), manifest)) {
            jarOutputStream.putNextEntry(new JarEntry(getPackageName(token) + "/" + token.getSimpleName() + "Impl.class"));
            Files.copy(compiledFile, jarOutputStream);
        } catch (IOException e) {
            throw new ImplerException("Can't write jar file");
        }
    }
    
    /**
     * Extended class of SimpleFileVisitor for deleting directories and all files in it
     */
    private final static class DELETER extends SimpleFileVisitor<Path> {
        @Override
        /**
         * Delete file if visit.
         *
         * @param file - path of file.
         * @param attrs - file attributes
         * @throws IOException if I/O happens
         * @return FileVisitResult.CONTINUE
         */
        public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
        }
        
        @Override
        /**
         * Delete directory if visit.
         *
         * @param file - path of dir.
         * @param exc - exceptions
         * @throws IOException if I/O happens
         * @return FileVisitResult.CONTINUE
         */
        public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
            Files.delete(dir);
            return FileVisitResult.CONTINUE;
        }
    };
    
    /**
     * Generate java code with realization of java interface specified by provided <strong>token</strong>.
     *
     * It is located at <strong>root</strong>/<strong>token</strong> package.
     * Generated class named as <strong>token</strong> class name with suffix <strong>Impl</strong>.
     *
     * Generated class must to compile but shouldn't to execute any logic.
     *
     * @param token type token to create implementation for.
     * @param root root directory.
     * @throws info.kgeorgiy.java.advanced.implementor.ImplerException when implementation cannot be
     * generated.
     */
    @Override
    public void implement(final Class<?> token, final Path root) throws ImplerException {
        checkArguments(token, root);

        final Path implementationFilePath = getFilePath(token, root);

        checkParentDir(implementationFilePath);

        try (BufferedWriter writer = Files.newBufferedWriter(implementationFilePath)) {
            writeImplementation(token, writer);
        } catch (IOException e) {
            throw new ImplerException("Problems with witting java implementation for " + token.getSimpleName(), e);
        }
    }
    
    /**
     * This function checks if arguments that was given to <strong>implement</strong> function aren't null and <strong>token</strong>
     * belongs to not private interface!
     *
     * @param token type token to create implementation for.
     * @param root root directory.
     * @throws info.kgeorgiy.java.advanced.implementor.ImplerException when one of the  arguments is null or
     * <strong>token</strong> isn't interface or it is private.
     */
    protected void checkArguments(final Class<?> token, final Path root) throws ImplerException {
        if (token == null || root == null) {
            throw new ImplerException("Argument of <implement> function can't be null");
        }
        if (!token.isInterface() || Modifier.isPrivate(token.getModifiers())) {
            throw new ImplerException("This realization of class implementing <Impler> can implement only interfaces");
        }
    }
    
    /**
     * This function returns java.nio.file.Path of class that will be th realization with implemented interface
     *
     * @param token type token to create implementation for.
     * @param root root directory.
     * @return Path to implementing file
     */
    private Path getFilePath(final Class<?> token, final Path root) {
        return root.resolve(getPackageName(token)).resolve(token.getSimpleName() + "Impl.java");
    }
    
    /**
     * Generate string showing package of implementing interface that is provided by token. String is row path. String is empty if token package is null.
     *
     * @param token type token to create implementation for.
     * @return String of class package, like uri
     */
    protected String getPackageName(final Class<?> token) {
        if (token.getPackage() == null) {
            return EMPTY_STRING;
        }

        return token.getPackageName().replace('.', '/');
    }
    
    /**
     * Function checks given dir. If parent dir of given dir exist, if not function creates it.
     *
     * @param implementationFilePath path of file that needed to be implement.
     * @throws info.kgeorgiy.java.advanced.implementor.ImplerException directory doesn't exist and function can't create it.
     */
    protected void checkParentDir(final Path implementationFilePath) throws ImplerException {
        try {
            if (implementationFilePath.getParent() != null && !Files.exists(implementationFilePath.getParent())) {
                Files.createDirectories(implementationFilePath.getParent());
            }
        } catch (IOException e) {
            throw new ImplerException("Can't create parent dir for implementing class(", e);
        }
    }
    
    /**
     * Function give good string code.
     *
     * @param string string reforming
     * @return String of good coding
     */
    private static String correctFormat(String string) {
        StringBuilder stringBuilder = new StringBuilder();
        for (int i = 0; i < string.length(); i++) {
            char symbol = string.charAt(i);
            stringBuilder.append(symbol < 128 ? symbol : "\\u" + String.format("%04x", (int) symbol));
        }
        return stringBuilder.toString();
    }
    
    /**
     * Function give code of class realization to BufferedWriter.
     *
     * @param token type token to create implementation for.
     * @param writer BufferedWriter that writing in needed file.
     * @throws java.io.IOException when writer has any problem
     *
     */
    private void writeImplementation(final Class<?> token, final BufferedWriter writer) throws IOException {
        writer.write(correctFormat(generatePackage(token) +
                generateClassBeginner(token) +
                generateMethods(token) +
                "}"));
    }
    
    /**
     * Function return String with generated code of class methods of <strong>token</strong> interface that needed to be Overwritten.
     *
     * @param token type token to create implementation for.
     * @return String of all class methods
     */
    private String generateMethods(final Class<?> token) {
        return getMethodsFromToken(token).stream()
                .filter(x -> Modifier.isAbstract(x.getModifiers())
                        && !Modifier.isPrivate(x.getModifiers()))
                .map(this::generateMethodString)
                .collect(Collectors.joining(LINE_SEPARATOR));
    }
    
    /**
     * Function return String with generated code of one method that was given as parameter.
     *
     * @param method that needed to be written.
     * @return String of method.
     */
    private String generateMethodString(final Method method) {
        StringBuilder text = new StringBuilder(getTabs(1));
        //Add modifier
        text.append(generateModifier(method.getModifiers()));
        //Add returning type
        text.append(method.getReturnType().getCanonicalName()).append(SPACE);
        //Add method name
        text.append(method.getName());
        //Add method parameters
        text.append(generateMethodParameters(method.getParameters()));
        //Add thrown exceptions
        text.append(generateExceptionsString(method.getExceptionTypes()));
        //Add method body
        text.append(generateMethodBody(method.getReturnType()));
        return text.toString();
    }
    
    /**
     * Function return String with body of method with default returning statement.
     *
     * @param returnType that returning by method.
     * @return String of method body
     */
    private String generateMethodBody(final Class<?> returnType) {
        return "{ " + LINE_SEPARATOR
                + getTabs(2) + "return"
                + generateReturningType(returnType)
                + ";" + LINE_SEPARATOR
                + getTabs(1) + "} " + LINE_SEPARATOR;
    }
    
    /**
     * String with default returning statement depends on returning type of class token <strong>returnType</strong>
     *
     * @param returnType that returning by method.
     * @return String of method returning default type
     */
    private String generateReturningType(final Class<?> returnType) {
        //Class
        if (!returnType.isPrimitive()) {
            return " null";
        }
        //Boolean
        if (returnType.equals(boolean.class)) {
            return " false";
        }
        //Void
        if (returnType.equals(void.class)) {
            return EMPTY_STRING;
        }
        //primitive number
        return " 0";
    }
    
    /**
     * String with Exceptions that throws by method.
     *
     * @param exceptionTypes array of throwing exceptions.
     * @return String of method exceptions
     */
    private String generateExceptionsString(final Class<?>[] exceptionTypes) {
        return exceptionTypes.length == 0 ? EMPTY_STRING :
                "throws "
                        + Arrays.stream(exceptionTypes)
                        .map(Class::getCanonicalName)
                        .collect(Collectors.joining(", "))
                        + SPACE;
    }
    
    /**
     * Function return String with parameters that is given to class method.
     *
     * @param parameters array of parameters of method.
     * @return String of method parameters
     */
    private String generateMethodParameters(final Parameter[] parameters) {
        return "(" + Arrays.stream(parameters)
                .map(x -> x.getType().getCanonicalName() + SPACE + x.getName())
                .collect(Collectors.joining(", "))
                + ") ";
    }
    
    /**
     * String with modifiers of given mask.
     *
     * @param modifiers mask with encrypted modifiers of method mask.
     * @return String of method modifiers
     *
     * @see java.lang.reflect.Modifier
     */
    private String generateModifier(int modifiers) {
        modifiers &= ~Modifier.ABSTRACT & ~Modifier.TRANSIENT;
        return Modifier.toString(modifiers).isEmpty() ? EMPTY_STRING : Modifier.toString(modifiers) + SPACE;
    }
    
    /**
     * Function return Array of methods that needed to be overwritten to implement token interface.
     *
     * @param token type token to create implementation for.
     * @return Set of unique methods of class
     */
    private Set<Method> getMethodsFromToken(final Class<?> token) {
        Set<Method> methods = new HashSet<>();

        Arrays.stream(token.getMethods())
                .filter(x -> !Modifier.isPrivate(x.getModifiers())
                        &&  Modifier.isAbstract(x.getModifiers()))
                .forEach(methods::add);

        return methods;
    }
    
    /**
     * String with java class beginning that is implementation of java token.
     *
     * @param token type token to create implementation for.
     * @return String class begging
     */
    private String generateClassBeginner(final Class<?> token) {
        return "public class " + token.getSimpleName()
                + "Impl implements " + token.getCanonicalName()
                + " {" + LINE_SEPARATOR;
    }
    
    /**
     * Function return String with java package line of class.
     *
     * @param token type token to create implementation for.
     * @return package line
     */
    private String generatePackage(final Class<?> token) {
        if (token.getPackageName().isEmpty()) {
            return EMPTY_STRING;
        }

        return "package "  + token.getPackageName() + ";" + LINE_SEPARATOR + LINE_SEPARATOR;
    }
    
    /**
     * Function return String with given number of tabs.
     *
     * @param i count of tabs.
     * @return line with tabs
     */
    private String getTabs(int i) {
        StringBuilder sb = new StringBuilder();
        while (i-- != 0) {
            String TAB = "\t";
            sb.append(TAB);
        }
        return sb.toString();
    }
}
