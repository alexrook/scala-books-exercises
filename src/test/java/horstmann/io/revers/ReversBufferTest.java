package horstmann.io.revers;

import org.junit.Ignore;
import org.junit.Test;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.*;


public class ReversBufferTest {

    @Ignore
    @Test
    public void testNonEmpty() {

        byte[] src = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};


        List<Byte> dst = new ArrayList<>(src.length);

        ByteBuffer buf = ByteBuffer.wrap(src);
//
//        Revers<Byte> instance = new ReversBuffer<>(buf, buf.limit() - 1);
//
//
//        while (instance.nonEmpty()) {
//            Byte obj = instance.head();
//            dst.add(obj);
//            instance = instance.tail();
//        }

        assertEquals(src.length, dst.size());

        Collections.reverse(dst);

        assertEquals(src.length, dst.size());

        for (int i = 0; i < src.length; i++) {
            assertEquals(src[i], dst.get(i).byteValue());
        }

    }

    @Test
    public void testEmpty() {

        byte[] src = {};


        List<Byte> dst = new ArrayList<>(src.length);
        ByteBuffer buf = ByteBuffer.wrap(src);
//        Revers<Byte> instance = new ReversBuffer(buf, buf.limit() - 1);
//
//        while (instance.nonEmpty()) {
//            Byte obj = instance.head();
//            dst.add(obj);
//            instance = instance.tail();
//        }
//
//        assertTrue(instance.isEmpty());

        assertEquals(src.length, dst.size());
        Collections.reverse(dst);
        assertEquals(src.length, dst.size());


    }


}
