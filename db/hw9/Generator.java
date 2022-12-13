import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Generator {
    private static final int DELIMITER_LENGTH = 100;
    private static final int COUNT_OF_PLANES = 4;
    private static final int COUNT_OF_DAYS = 7;
    
    private static final int CUSTOMERS_COUNT = 30;
    
    public static void main(String[] args) {
        //createFlightsAndPlanes();
        //createSeats();
        createUsers();
    }
    
    private static void createUsers() {
        System.out.println(delimiter("Customers"));
        
        final StringBuilder customers = new StringBuilder();
    
        for (int i = 0; i < CUSTOMERS_COUNT; i++) {
            createCustomer(i, customers);
            customers.append(",\n");
        }
        
        customers.deleteCharAt(customers.length() - 1);
        customers.deleteCharAt(customers.length() - 1);
        
        customers.append(";");
    
        System.out.println(customers);
    }
    
    private static void createCustomer(final int i, final StringBuilder customers) {
        Random r = new Random();
        customers.append("(").append(i).append(", '");
        for (int k = 0; k < 2; k++) {
            for (int j = 0; j < r.nextInt(5) + 5; j++) {
                customers.append((char)(r.nextInt('z' - 'a') + 'a'));
            }
            customers.append(" ");
        }
        customers.deleteCharAt(customers.length() - 1);
        customers.append("', '");
    
        for (int j = 0; j < r.nextInt(10) + 5; j++) {
            customers.append(r.nextInt(10));
        }
        
        customers.append("')");
    }
    
    
    
    private static void createFlightsAndPlanes() {
        System.out.println(delimiter("Planes"));
        
        final StringBuilder planes = new StringBuilder();
        
        for (int i = 0; i < COUNT_OF_PLANES; i++) {
            planes.append("(")
                    .append(i)
                    .append(")")
                    .append(",")
                    .append("\n");
        }
        
        planes.deleteCharAt(planes.length() - 2);
        planes.deleteCharAt(planes.length() - 1);
        
        planes.append(";");
        
        System.out.println(planes);
        
        System.out.println(delimiter("Flights"));
        
        final StringBuilder flights = new StringBuilder();
        
        for (int i = 0; i < COUNT_OF_DAYS; i++) {
            for (int j = 0; j < COUNT_OF_PLANES; j++) {
                flights.append("(")
                        .append(i * COUNT_OF_PLANES + j)
                        .append(", ")
                        .append("now() + interval '")
                        .append(i)
                        .append(" days', ")
                        .append(j)
                        .append("),\n");
            }
        }
        
        flights.deleteCharAt(flights.length() - 2);
        flights.deleteCharAt(flights.length() - 1);
        
        flights.append(";");
        
        System.out.println(flights);
    }
    
    private static String delimiter(String s) {
        StringBuilder stringBuilder = new StringBuilder(s);
        
        while (stringBuilder.length() != DELIMITER_LENGTH) {
            stringBuilder.append("=");
        }
        
        return stringBuilder.toString();
    }
    
    private static void createSeats() {
        System.out.println(delimiter("Seats"));
        
        final StringBuilder seats = new StringBuilder();
        
        for (int i = 0; i < COUNT_OF_PLANES; i++) {
            List<String> seatsString = seats(randomePlaneLength());
            
            for (String seat : seatsString) {
                seats.append("(")
                        .append(i)
                        .append(", '")
                        .append(seat)
                        .append("'),\n");
            }
        }
        
        seats.deleteCharAt(seats.length() - 1);
        seats.deleteCharAt(seats.length() - 1);
    
        seats.append(";");
        
        System.out.println(seats);
    }
    
    private static List<String> seats(int n) {
        final List<String> seats = new ArrayList<>();
        final List<String> places = List.of("A", "B", "C", "D", "E", "F");
        
        for (int i = 0; i < n; i++) {
            for (String place : places) {
                seats.add(i + place);
            }
            
        }
        
        return seats;
    }
    
    private static int randomePlaneLength() {
        return ((int) (Math.random() * 10)) + 5;
    }
}
