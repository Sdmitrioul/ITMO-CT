package md2html;

import java.io.*;
import java.util.NoSuchElementException;

public class Scanner {
    private BufferedReader reader;
    private String line;
    private boolean readed;
    private int index = 0;
    private int position = 0;
    private String mark;

    public Scanner(InputStream in) throws UnsupportedEncodingException {
        try{
            reader = new BufferedReader(new InputStreamReader(in, "utf8"));
        } catch (UnsupportedEncodingException e){
            throw new UnsupportedEncodingException("Encode error :" + e.getMessage());
        }
    }

    public Scanner(String s, String charset) throws FileNotFoundException, UnsupportedEncodingException  {
        try{
            reader = new BufferedReader(new InputStreamReader(new FileInputStream(s), charset));
        } catch (FileNotFoundException e){
            throw new FileNotFoundException("File not found :" + e.getMessage());
        } catch (UnsupportedEncodingException e){
            throw new UnsupportedEncodingException("Encode error :" + e.getMessage());
        }
    }

    public Scanner(String s) {
        line = s;
        position = 0;
    }

    public boolean endLine() {
        if (position < line.length()) {
            return false;
        } else {
            return true;
        }
    }

    public StringBuilder getStringBeforeMark() {
        StringBuilder stringBuilder = new StringBuilder();
        while (!endLine() && !markSymbol(line.charAt(position))) {
            stringBuilder.append(line.charAt(position));
            position++;
        }
        StringBuilder mark = new StringBuilder();
        if (!endLine()) {
            mark.append(line.charAt(position));
            position++;
        }
        if (!endLine() && markSymbol(line.charAt(position))) {
            if (line.charAt(position) == mark.charAt(0)) {
                mark.append(line.charAt(position));
            }
            position++;
        }
        this.mark = mark.toString();
        return stringBuilder;
    }

    public String nextMark(){
        return mark;
    }

    private boolean markSymbol(char s){
        return s == '+' || s == '*' || s == '_' || s == '-' || s == '`';
    }

    public boolean hasNextInt() {
        while (index < line.length() && Character.isWhitespace(line.charAt(index))){
            index++;
        }
        return index != line.length();
    }
    
    public boolean hasNextWord() {
        while (index < line.length() && !isWordSymbol(line.charAt(index))){
            index++;
            }
        return index != line.length();
    }
    
    private boolean isWordSymbol(char c) {
        return Character.isLetter(c) || Character.getType(c) == Character.DASH_PUNCTUATION ||  c == '\'';
    }
    
    public int getNextInt() {
        if (!hasNextInt()) {
            throw new NoSuchElementException();
        }
        int begin = index;
        while (!Character.isWhitespace(line.charAt(index))){
            index++;
            if (index == line.length()){
                break;
            }
        }
        return Integer.parseInt(line.substring(begin, index));
    }
    
    public String getNextWord() {
        if (!hasNextWord()) {
            throw new NoSuchElementException();
        }
            int begin = index;
            while (isWordSymbol(line.charAt(index))){
                index++;
                if (index == line.length()){
                    break;
                }
            }
            return line.substring(begin, index);
    }
    
    public boolean hasLine() throws IOException {
        try{
            index = 0;
            readed = true;
            line = reader.readLine();
        } catch (IOException e) {
            throw new IOException("Input error in Scanner.hasLine(): " + e.getMessage());
        }
        return line != null;
    }

    public String getLine() throws IOException {
        if (readed) {
            readed = false;
            return line;
        } else {
            if (hasLine()) {
                readed = false;
                return line;
            } else {
                return "";
            }
        }
    }

    public void close() throws IOException {
        try{
            reader.close();
        } catch(IOException e) {
            throw new IOException("Eror: has problem in scanner.close() " + e.getMessage());
        }
    }
}
