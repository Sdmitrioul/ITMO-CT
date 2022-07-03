import java.util.ArrayList;
import java.util.Collections;

public class Reverse implements Sorting {
    private ArrayList<String> list;

    public Reverse(ArrayList<String> list) {
        this.list = list;
    }

    @Override
    public void sort() {
        Collections.sort(list);
    }

    @Override
    public void print() {
        for (int i = list.size() - 1; i >= 0; i--) {
            System.out.println(list.get(i));
        }
    }

    @Override
    public ArrayList<String> printInFile() {
        return list;
    }
}
