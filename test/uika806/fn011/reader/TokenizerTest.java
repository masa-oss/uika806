package uika806.fn011.reader;

import uika806.port.CodepointLispStream;
import java.util.List;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author hemmi
 */
public class TokenizerTest {

    public TokenizerTest() {
    }

    Tokenizer newInstance(String str) {

        CodepointLispStream stream = null;
        try {
            stream = CodepointLispStream.fromUtf8(str);
        } catch (Exception ex) {
            throw new RuntimeException("", ex);
        }
        Tokenizer tk = new Tokenizer(stream);
        return tk;
    }

    @Test
    public void testNext() {

        System.out.println(" String r7rs p.43");

        //                                "\n"
        Tokenizer instance = newInstance("\"\\n\"");
        Token result = instance.next();

        System.out.println("" + result);

        int type = '"';
        assertEquals(type, result.id);

        List<Integer> str = result.str;

        assertEquals(1, str.size());

        assertEquals((long) 10, (long) str.get(0));
    }

    @Test
    public void testNext2() {

        System.out.println(" Comment");

        //                                ;123\r\n
        Tokenizer instance = newInstance(";123\r\n");
        Token result = instance.next();

        System.out.println("" + result);

        int type = ';';
        assertEquals(type, result.id);

        List<Integer> str = result.str;

        assertEquals(5, str.size());

        result = instance.next();

        System.out.println("" + result);

        assertEquals((int) -1, (int) result.id);
    }

    @Test
    public void testNext_String_LF() {

        System.out.println(" String r7rs p.43");

        //                                "\   \n a"
        Tokenizer instance = newInstance("\"\\   \n a\"");
        Token result = instance.next();

        System.out.println("" + result);

        int type = '"';
        assertEquals(type, result.id);

        List<Integer> str = result.str;

        assertEquals(1, str.size());

        assertEquals((long) 'a', (long) str.get(0));
    }

    @Test
    public void testNext_String_SharpX() {

        System.out.println(" #\\x");

        //                                #\x
        Tokenizer instance = newInstance("#\\x");
        Token result = instance.next();

        System.out.println("" + result);

        int type = '#';
        assertEquals(type, result.id);

        List<Integer> str = result.str;

        assertEquals(2, str.size());

        assertEquals((long) 'x', (long) str.get(1));
    }

    @Test
    public void testNext_Dot_3() {

        System.out.println(" .3");

        //                                .3
        Tokenizer instance = newInstance(".3");
        Token result = instance.next();

        System.out.println("" + result);

        int type = 'N';
        assertEquals(type, result.id);
        
        List<Integer> str = result.str;

        assertEquals( 2, str.size());
        
        assertEquals((long)'.', (long)str.get(0));
        assertEquals((long)'3', (long)str.get(1));
    }



    @Test
    public void testNext_Sharp_Semic() {

        System.out.println("#;");

        //                                #;a 1
        Tokenizer instance = newInstance("#;a 1");
        Token result = instance.next();

        System.out.println("" + result);

        int type = 0xa6;
        assertEquals(type, result.id);
    }


    @Test
    public void testNext_String_LF2() {

        System.out.println(" String r7rs p.43   test-17");

        //                                "1\  \t \n \t cont"
        Tokenizer instance = newInstance("\"1\\ \t  \n \t cont\"");
        Token result = instance.next();

        System.out.println("" + result);

        int type = '"';
        assertEquals(type, result.id);

        List<Integer> str = result.str;

        assertEquals(5, str.size());

        assertEquals((long) '1', (long) str.get(0));
        assertEquals((long) 'c', (long) str.get(1));
        assertEquals((long) 'o', (long) str.get(2));
        assertEquals((long) 'n', (long) str.get(3));
        assertEquals((long) 't', (long) str.get(4));
    }


    @Test
    public void testNext_String_LF3() {

        System.out.println(" String r7rs p.43   test17.scm");

        //                                "1\  \t \n \t cont"
        Tokenizer instance = newInstance("\"1\\ \t  \n \t \n\nline\"");
        Token result = instance.next();

        System.out.println("" + result);

        int type = '"';
        assertEquals(type, result.id);

        List<Integer> str = result.str;

        assertEquals(7, str.size());

        assertEquals((long) '1', (long) str.get(0));
        assertEquals((long) 10, (long) str.get(1));
        assertEquals((long) 10, (long) str.get(2));
        assertEquals((long) 'l', (long) str.get(3));
        assertEquals((long) 'i', (long) str.get(4));
        assertEquals((long) 'n', (long) str.get(5));
        assertEquals((long) 'e', (long) str.get(6));
    }


}
