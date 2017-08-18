import java.util.ArrayList;
import java.util.List;

/**
 * Created by moroz on 17.05.17.
 */
public class JSection8 {

    public final int a = 1;

    public void printA() {
        System.out.println("JSection8,a:" + a);
    }

    public static void main(String[] args) {
        System.out.println(new String("aaa").equals(new String("aaa")));

        JSection8Ex ex = new JSection8Ex();

        ex.printA();

        List<JSection8> list = new ArrayList<>(3);

        for (int i = 0; i < 3; i++) {
            list.add(new JSection8Ex());
        }

        for (JSection8 section : list) {
            section.printA();
        }
    }

}

class JSection8Ex extends JSection8 {

    public final int a = 2;

    public void printA() {
        System.out.println("JSection8Ex,a:" + a);
    }

}
