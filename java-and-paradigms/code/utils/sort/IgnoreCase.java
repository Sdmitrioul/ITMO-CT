import java.util.*;

public class IgnoreCase implements Sorting {
    private Map<String, String> map;
    private ArrayList<Map.Entry<String, String>> list;

    public IgnoreCase(ArrayList<String> list) {
        map = new LinkedHashMap<>();
        for (String iterator : list) {
            map.put(iterator, iterator.toLowerCase());
        }
    }

    @Override
    public void sort() {
        list = new ArrayList<>(map.entrySet());
        Collections.sort(list, new Comparator<Map.Entry<String, String>>() {
            @Override
            public int compare(Map.Entry<String, String> o1, Map.Entry<String, String> o2) {
                return o1.getValue().compareTo(o2.getValue());
            }
        });
    }

    @Override
    public void print() {
        for (int i = 0; i < list.size(); i++) {
            System.out.println(list.get(i).getKey());
        }
    }

    @Override
    public ArrayList<String> printInFile() {
        ArrayList<String> ret = new ArrayList<>();
        for (int i = 0; i < list.size(); i++) {
            ret.add(list.get(i).getKey());
        }
        return ret;
    }
}
