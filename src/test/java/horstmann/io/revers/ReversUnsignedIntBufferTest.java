package horstmann.io.revers;

import org.junit.Test;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class ReversUnsignedIntBufferTest {

    @Test
    public void testNonEmpty() {

        byte[] src = {-1, -128,127, 1};
        int[] expected = {1, 127,128, 255};

        List<Integer> dst = new ArrayList<>(src.length);

        ByteBuffer buf = ByteBuffer.wrap(src);
//
//        Revers instance
//                = new ReversUnsignedIntBuffer(buf, buf.limit() - 1);
//
//
//        while (instance.nonEmpty()) {
//            Integer obj = (Integer) instance.head();
//            dst.add(obj);
//            instance = instance.tail();
//        }
//
//        assertEquals(src.length, dst.size());
//
//        for (int k=0;k<expected.length;k++) {
//            assertEquals(expected[k],dst.get(k).intValue());
//        }

    }
}
