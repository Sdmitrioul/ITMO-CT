import java.util.ArrayList;
import java.util.*;

public class Sort implements Sorting {
    private ArrayList<String> list;

    public Sort(ArrayList<String> list) {
        this.list = list;
    }

    @Override
    public void sort() {
        Collections.sort(list);
    }

    @Override
    public ArrayList<String> printInFile() {
        return list;
    }

    @Override
    public void print() {
        for (String iterator : list) {
            System.out.println(iterator);
        }
    }
}
