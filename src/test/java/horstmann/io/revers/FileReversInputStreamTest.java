package horstmann.io.revers;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import static org.junit.Assert.*;

import java.io.*;
import java.util.ArrayList;
import java.util.List;


public class FileReversInputStreamTest {


    static byte[] testData01 = "abcdef".getBytes();
    static byte[] revertTestData01 = new byte[testData01.length];

    static byte[] testData02 = "linДОТЛeX".getBytes();
    static byte[] revertTestData02 = new byte[testData02.length];

    static File tmpFile01, tmpFile02;

    @BeforeClass
    public static void init() {
        try {
            tmpFile01 = File.createTempFile("test-revert-file-input-stream-",
                    "-data.01");

            try (OutputStream data =
                         new FileOutputStream(tmpFile01)) {
                data.write(testData01);
            }

            for (int i = 0; i < testData01.length; i++) {
                revertTestData01[testData01.length - 1 - i] = testData01[i];
            }


            tmpFile02 = File.createTempFile("test-revert-file-input-stream-",
                    "-data.02");
            try (OutputStream data =
                         new FileOutputStream(tmpFile02)) {
                data.write(testData02);
            }

            for (int i = 0; i < testData02.length; i++) {
                revertTestData02[testData02.length - 1 - i] = testData02[i];
            }
        } catch (IOException e) {
            System.out.println("an exception while writing test data:" + e.getMessage());
        }
    }

    @AfterClass
    public static void destroy() {
        tmpFile01.delete();
        tmpFile02.delete();
    }

    @Test
    public void testReadDefaultReadBufferSize01() {

//        try (InputStream r = FileReversInputStream.apply(tmpFile01)) {
//            int c;
//            List<Integer> buf = new ArrayList<>(revertTestData01.length + 1);
//            while ((c = r.read()) != -1) {
//                buf.add(c);
//            }
//            for (int k = 0; k < revertTestData01.length; k++) {
//                assertEquals(revertTestData01[k], buf.get(k).byteValue());
//            }
//
//        } catch (IOException e) {
//            System.out.println("an exception while writing test data:" + e.getMessage());
//        }
    }

    @Test
    public void l1() {
        int i=255;
        byte b=(byte) i;
        int i2=Byte.toUnsignedInt(b);
        System.out.println(i2);
//        try (InputStream r = new FileInputStream(tmpFile02)) {
//            int c;
//            List<Integer> buf = new ArrayList<>(revertTestData02.length + 1);
//            while ((c = r.read()) != -1) {
//                buf.add(c);
//            }
//            for (Integer i : buf) {
//                System.out.println(i + ";" + ((byte) i.intValue()));
//            }
//
//        } catch (IOException e) {
//            System.out.println("an exception while writing test data:" + e.getMessage());
//        }

    }

    @Test
    public void testReadDefaultReadBufferSize02() {


//        try (InputStream r = FileReversInputStream.apply(tmpFile02)) {
//            int c;
//            List<Integer> buf = new ArrayList<>(revertTestData02.length + 1);
//            while ((c = r.read()) != -1) {
//                buf.add(c);
//            }
//            for (int k = 0; k < revertTestData02.length; k++) {
//             //   System.out.println(buf.get(k));
//                assertEquals(revertTestData02[k], buf.get(k).byteValue());
//            }
//
//        } catch (IOException e) {
//            System.out.println("an exception while writing test data:" + e.getMessage());
//        }

    }

    @Test
    public void testReadCustomReadBufferSize01() {
//        try (InputStream r = FileReversInputStream.apply(tmpFile01, 1)) {
//            int c;
//            List<Integer> buf = new ArrayList<>(revertTestData01.length + 1);
//            while ((c = r.read()) > 0) {
//                buf.add(c);
//            }
//            for (int k = 0; k < revertTestData01.length; k++) {
//                assertEquals(revertTestData01[k], buf.get(k).byteValue());
//            }
//
//        } catch (IOException e) {
//            System.out.println("an exception while writing test data:" + e.getMessage());
//        }
    }
}
