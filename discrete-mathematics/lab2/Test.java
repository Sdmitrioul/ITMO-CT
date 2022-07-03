import java.util.*;

public class Test {
    public static void main(String[] args) {
        System.out.println(dblLinear(10));
    }
    public static int dblLinear (int n) {
        TreeSet<Integer> list = new TreeSet<>();
        list.add(1);
        int site = 1;
        list.
        while (list.size() <= n * n) {
            int last = list.get(list.size() - site);
            list.add(2 * last + 1);
            list.add(3 * last + 1);
            site++;
        }
        int last = list.get(list.size() - site);
        list.add(2 * last + 1);
        list.add(3 * last + 1);
        site++;
        Collections.sort(list);
        return list.get(n);
    }
}
