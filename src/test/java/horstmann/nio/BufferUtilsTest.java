package horstmann.nio;
import org.junit.Test;

import static org.junit.Assert.*;

import java.nio.ByteBuffer;
import java.util.Arrays;

public class BufferUtilsTest {

    @Test
    public void testCompareBuffers() {

//        ByteBuffer model = ByteBuffer.allocate(12);
//        ByteBuffer source = ByteBuffer.allocate(12);
//
//        assertTrue(BufferUtils.compareBuffersAll(model,
//                source, model.limit(), source.limit())); //empty buffers equals
//
//        model.clear();
//        source.clear();
//
//        byte[] data = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};
//
//        model.put(data);
//        source.put(data);
//        assertTrue(BufferUtils.compareBuffersAll(model, source, model.limit(), source.limit()));
//        assertTrue(
//                BufferUtils.compareBuffersHead(model, source,
//                        source.limit())); //must be true for same buffers
//
//
//        //source < model
//        model.clear();
//        source.clear();
//
//        byte[] dat = Arrays.copyOf(data, 7);
//        source.put(dat); //partial
//        model.put(data);
//
//        model.flip();
//        source.flip();
//
//        int modelPos = model.position(),
//                modelLim = model.limit(),
//                sourcePos = source.position(),
//                sourceLim = source.limit();
//
//        assertTrue(BufferUtils.compareBuffersHead(model, source, sourceLim));
//        assertTrue(modelPos == model.position()); //assert what nothing in touch
//        assertTrue(modelLim == model.limit());
//        assertTrue(sourcePos == source.position());
//        assertTrue(sourceLim == source.limit());
//
//        assertFalse(BufferUtils.compareBuffersAll(model, source,
//                modelLim, sourceLim));
//        assertTrue(modelPos == model.position());
//        assertTrue(modelLim == model.limit());
//        assertTrue(sourcePos == source.position());
//        assertTrue(sourceLim == source.limit());
//
//
//        //source > model
//        model.clear();
//        source.clear();
//
//        dat = Arrays.copyOf(data, 7);
//        source.put(data);
//        model.put(dat);//partial
//
//        model.flip();
//        source.flip();
//
//        modelPos = model.position();
//        modelLim = model.limit();
//        sourcePos = source.position();
//        sourceLim = source.limit();
//
//        assertFalse(BufferUtils.compareBuffersHead(model, source, sourceLim));
//        assertTrue(BufferUtils.compareBuffersHead(source, model, modelLim)); //the opposite is true
//        assertTrue(modelPos == model.position()); //assert what nothing in touch
//        assertTrue(modelLim == model.limit());
//        assertTrue(sourcePos == source.position());
//        assertTrue(sourceLim == source.limit());
//
//        assertFalse(BufferUtils.compareBuffersAll(model, source,
//                modelLim, sourceLim));
//        assertTrue(modelPos == model.position());
//        assertTrue(modelLim == model.limit());
//        assertTrue(sourcePos == source.position());
//        assertTrue(sourceLim == source.limit());


    }

    @Test
    public void testDuplicateBuffer() {
//        ByteBuffer model = ByteBuffer.allocate(12);
//
//        byte[] data = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};
//
//        model.put(data);
//
//        ByteBuffer copy = BufferUtils.duplicateBuffer(model);
//
//        assertEquals(model.limit(), copy.limit());
//        assertFalse(model == copy);


    }

}
