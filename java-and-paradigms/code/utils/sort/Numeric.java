import java.util.*;

public class Numeric implements Sorting {
    private ArrayList<Map.Entry<String, Integer>> list;
    private Map<String, Integer> map;

    public Numeric(ArrayList<String> list) {
        map = new LinkedHashMap<>();
        for (String iterator : list) {
            int beginIndex = 0;
            while (!Character.isDigit(iterator.charAt(beginIndex))) {
                beginIndex++;
            }
            int endIndex = beginIndex;
            while (Character.isDigit(iterator.charAt(endIndex))) {
                endIndex++;
            }
            map.put(iterator, Integer.valueOf(iterator.substring(beginIndex, endIndex)));
        }
    }

    @Override
    public void sort() {
        list = new ArrayList<>(map.entrySet());
        Collections.sort(list, new Comparator<Map.Entry<String, Integer>>() {
            @Override
            public int compare(Map.Entry<String, Integer> o1, Map.Entry<String, Integer> o2) {
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
