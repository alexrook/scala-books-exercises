package horstmann;

import java.util.ArrayList;
import java.util.List;

public class JSection5_8 {

    class Inner {
        private String name;
        private List<Inner> contacts = new ArrayList<>(5);

        public Inner(String name) {
            this.name = name;
        }

        public void addContact(Inner contact) {
            this.contacts.add(contact);
        }

        @Override
        public String toString() {
            return this.name+","+JSection5_8.this;
        }
    }

    public static void main(String[] args) {
        JSection5_8 outer1 = new JSection5_8();
        Inner i1 = outer1.new Inner("outer1:i1");

        System.out.println(i1.getClass().getName());

        JSection5_8 outer2 = new JSection5_8();
        Inner i2 = outer2.new Inner("outer2:i2");

        System.out.println(i2.getClass().getName());

        System.out.println(i2.getClass() == i1.getClass());

        i1.addContact(i2);//ok for different outers
        i2.addContact(i1);
        printList(i1.contacts);
        printList(i2.contacts);

    }

    public static void printList(List list){
        for(Object item:list) {
            System.out.print(item+" ");
        }
    }
}
