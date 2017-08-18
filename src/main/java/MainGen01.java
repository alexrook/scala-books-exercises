import java.util.ArrayList;
import java.util.List;

public class MainGen01 {

    public static void main(String[] args) {
        Container01 c011 = new Container01(new Camera("samsung", 34.5));
        System.out.println(c011.toLine());

        c011.setItem(new Phone("sonyercison", 12.3));
        System.out.println(c011.toLine());

        //  c011.setItem(new String("aaaa")); don't compile
        System.out.println(c011.toLine());

        Container01<Camera> c012 = new Container01(new Camera("lg", 77.1));

        // c012.setItem(new Phone("nokia",45.7)); d'ont compile

        //   testFind();
        testCopy1();

    }

    public static final void copy(List<? extends Product> src, List<? super Product> dst) {
        for (Product p : src) {
            dst.add(p);
        }
    }

    public static final void testCopy1() {
        List<Camera> cameras1 = new ArrayList<Camera>();
        List<Product> phones1 = new ArrayList<Product>();

        List<? extends Product> cameras = cameras1;
        List<? super Product> phones = phones1;

        cameras1.add(new Camera("ca", 1));
        phones1.add(new Phone("pa", 1));

        copy(cameras, phones);

        System.out.println(phones);
    }

    public static final void testFind() {

        List<Camera> cameras = new ArrayList<>();
        List<Phone> phones = new ArrayList<>();

        System.out.println(find(cameras, new Camera("a", 1)));
        System.out.println(find(cameras, new Phone("b", 1)));

        System.out.println(find(phones, new Camera("a", 1)));
        System.out.println(find(phones, new Phone("c", 1)));


        System.out.println(find1(cameras, new Camera("a", 1)));
        // System.out.println(find1(cameras, new Phone("b", 1)));

        // System.out.println(find1(phones, new Camera("a", 1)));
        System.out.println(find1(phones, new Phone("c", 1)));

    }

    public static String find(List<? extends Product> list, Product prod) {
        for (Product p : list) {
            if (p.equals(prod))
                return prod.getName();
        }
        return null;
    }

    public static final <T extends Product> String find1(List<T> list, T prod) {
        return find(list, prod);
    }


}


abstract class Product {
    abstract  String getName();

    abstract double getPrice();

    public String toString() {
        return getClass().getName()+":"+ getName() + ":" + getPrice();
    }
}

class Camera extends Product {

    private String name;
    private double price;

    public Camera(String name, double price) {
        this.name = name;
        this.price = price;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public double getPrice() {
        return price;
    }
}

class Phone extends Product {

    private String name;
    private double price;

    public Phone(String name, double price) {
        this.name = name;
        this.price = price;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public double getPrice() {
        return price;
    }
}

class Container01<T extends Product> {

    private T item;

    public Container01(T item) {
        this.item = item;
    }

    public String toLine() {
        return item.getName() + ":" + item.getPrice();
    }

    public void setItem(T item) {
        this.item = item;
    }
}


class Container02<T extends Product & Comparable<T>> {

    private T item;

    public Container02(T item) {
        this.item = item;
    }

    public String toLine() {
        return item.getName() + ":" + item.getPrice();
    }

    public void setItem(T item) {
        this.item = item;
    }

    public int compare(T other) {
        return item.compareTo(other);
    }
}