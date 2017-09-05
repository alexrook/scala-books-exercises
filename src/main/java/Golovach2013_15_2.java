package golovach;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Arrays;

public class Golovach2013_15_2 {


    public static void main(String[] args) throws IOException {

        ByteArrayOutputStream data = new ByteArrayOutputStream(120);
        DataOutputStream dst = new DataOutputStream(data);

        StringBuffer buf = new StringBuffer();

        for (int i = 0; i < 257; i++) {
            buf.append("a");
        }

        dst.writeUTF("");
        dst.writeUTF(buf.toString());
        dst.flush();
        printA(data.toByteArray());

        dst.close();

        byte[] A = "A".getBytes("UTF-8");
        printA(A);
        byte[] AA = "AAA".getBytes("UTF-8");
        printA(AA);

    }

    public static void printA(byte[] a) {
        System.out.println("array data:");
        System.out.println(Arrays.toString(a));
    }
}
