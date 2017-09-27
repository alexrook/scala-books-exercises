package horstmann.io.revers;

import org.junit.Test;
import scala.Option;

import scala.collection.immutable.List;

import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;

import static org.junit.Assert.*;

public class FileReversLineReaderTest {


    @Test
    public void testMatchTailsWhole() {

        Integer[] model = {1, 2, 3, 4, 5, 6, 7};

        Integer[] s1 = {4, 5, 6, 7};
        Integer[] s2 = {1, 4, 5, 6, 7};
        Integer[] s3 = {1, 2, 3, 4, 5, 6, 7};
        Integer[] s4 = {0, 1, 2, 3, 4, 5, 6, 7};

        assertTrue(FileReversLineReader.matchTails(model, s1, 0));
        assertFalse(FileReversLineReader.matchTails(model, s2, 0));
        assertTrue(FileReversLineReader.matchTails(model, s3, 0));
        assertFalse(FileReversLineReader.matchTails(model, s4, 0));
    }

    @Test
    public void testMatchTailsByPos() {

        Integer[] model = {1, 2, 3, 4, 5, 6, 7};

        Integer[] s1 = {4, 5, 6, 7};
        Integer[] s2 = {42, 12,/*совпадение с позиции 2*/ 5, 6, 7};
        Integer[] s3 = {1, 2, 3, 4, 5, 6, 7};
        Integer[] s4 = {0, 1, 2, 3, 4, 5, 6, 7};

        assertTrue(FileReversLineReader.matchTails(model, s1, 1));
        assertTrue(FileReversLineReader.matchTails(model, s2, 2));
        assertTrue(FileReversLineReader.matchTails(model, s3, 0));
        //false because src.length>model.length
        assertFalse(FileReversLineReader.matchTails(model, s4, 1));
    }


    //TODO add test asserts & more testing here
    @Test
    public void testReadLine01() throws UnsupportedEncodingException {
        byte[] lineSeparator = "\n".getBytes("UTF-8");

        FileReversLineReader instance = new FileReversLineReader(lineSeparator,
                FileReversInputStream.apply("out/production/resources/section9.q1.data"));

        Option<List<Object>> optionLine = instance.readLine();
        assertTrue(optionLine.nonEmpty());//line with 0 chars also match

        while (optionLine.nonEmpty()) {

            List<Object> chars = optionLine.get();

            byte[] bytes = new byte[chars.length()];
            chars.foldLeft(0, (i, o) -> {
                bytes[i] = ((Integer) o).byteValue();
                return i + 1;
            });

            String s = new String(bytes, Charset.forName("UTF-8"));
            s = s.isEmpty() ? "empty line" : s;
            System.out.print(s);
            System.out.println("<----------->");
            optionLine = instance.readLine();
        }


    }
}
