package horstmann;


public class JSection5 {

    public static void main(String[] args) {

        Section5.Test t=new Section5.Test("test inner class in Scala object");
        System.out.println(t.s());

        //no  PersonWithDefs() without args
        PersonWithDefs p = new PersonWithDefs("Svetlana", 19);
        System.out.println(p.name() + ":" + p.age());
    }
}
