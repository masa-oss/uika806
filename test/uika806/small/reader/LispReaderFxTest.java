package uika806.small.reader;

import java.util.ArrayList;
import uika806.reader.LispReaderFx;
import org.junit.Test;
import static org.junit.Assert.*;
import uika806.fn011.reader.Tokenizer;
import uika806.port.CodepointLispStream;
import uika806.err.ReaderException;
import uika806.reader.LispReaderEx;
import uika806.objects.Cell;
import uika806.kernel.RT;
import uika806.objects.SString;
import uika806.objects.Complex;
import uika806.objects.EndOfFile;
import uika806.objects.Ratio;
import uika806.objects.SChar;
import uika806.objects.SSymbol;
import uika806.objects.Undef;
import uika806.port.StringInputPort;

/**
 *
 * @author hemmi
 */
public class LispReaderFxTest {

    public LispReaderFxTest() {
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

    //Test
    public void testRead() {
        System.out.println("read  #\\x");
        
        Tokenizer tk = newInstance("#\\x");
        boolean eofError = false;
        Object eofval = null;
        LispReaderEx instance = new LispReaderEx(tk);
        Object result = instance.read( eofError, eofval);
        
     //   System.out.println("result=" + result);
        assertTrue (result instanceof SChar);
        SChar ch = (SChar) result;
        int cp = ch.getCodepoint();
        
        int expResult = 'x';
        assertEquals(expResult, cp);
    }
    
    
    //Test
    public void testFixMe() {
        System.out.println("6.7. 文字列");
        System.out.println("\\<行内空白>*<行末><行内空白>* : nothing");

        Tokenizer tk = newInstance("\"\\     \n     \na\"");
        boolean eofError = false;
        Object eofval = null;
        LispReaderFx instance = new LispReaderFx(tk);
        Object result = instance.read(eofError, eofval);

        System.out.println("result=" + result);
        assertTrue(result instanceof SString);
        SString ss = (SString) result;

        assertEquals("\na", ss.toString());
    }

    //Test
    public void testComplex1() {
        System.out.println("complex1");

        Tokenizer tk = newInstance("1+2i");
        boolean eofError = false;
        Object eofval = null;
        LispReaderFx instance = new LispReaderFx(tk);
        Object result = instance.read(eofError, eofval);

        System.out.println("result=" + result);

        assertTrue(result instanceof Complex);
    }

    //Test
    public void testRatio1() {
        System.out.println("ratio1");

        Tokenizer tk = newInstance("1/2");
        boolean eofError = false;
        Object eofval = null;
        LispReaderFx instance = new LispReaderFx(tk);
        Object result = instance.read(eofError, eofval);

        System.out.println("result=" + result);

        assertTrue(result instanceof Ratio);
    }

    //Test
    public void testComplex2() {
        System.out.println("complex2");

        Tokenizer tk = newInstance("0.54030230586814+0.841470984807897i");
        boolean eofError = false;
        Object eofval = null;
        LispReaderFx instance = new LispReaderFx(tk);
        Object result = instance.read(eofError, eofval);

        System.out.println("result=" + result);

        assertTrue(result instanceof Complex);
    }

    //Test
    public void testComplex3() {
        System.out.println("complex3");

        Tokenizer tk = newInstance("3.0+inf.0i");
        boolean eofError = false;
        Object eofval = null;
        LispReaderFx instance = new LispReaderFx(tk);
        Object result = instance.read(eofError, eofval);

        System.out.println("result=" + result);

        assertTrue(result instanceof Complex);
        Complex co = (Complex) result;

        System.out.println("111) " + co.im());
        assertTrue(Double.isInfinite(co.im()));

        assertTrue(co.im() > 0.0);
    }

    //Test
    public void testComplex4() {
        System.out.println("complex4");

        Tokenizer tk = newInstance("+nan.0+5.0i");
        boolean eofError = false;
        Object eofval = null;
        LispReaderFx instance = new LispReaderFx(tk);
        Object result = instance.read(eofError, eofval);

        System.out.println("result=" + result);

        assertTrue(result instanceof Complex);
        Complex co = (Complex) result;

        System.out.println("111) " + co.im());
        assertTrue(Double.isNaN(co.re()));

    }

    //Test
    public void testDouble1() {
        System.out.println("double-1");

        Tokenizer tk = newInstance("3.");
        boolean eofError = false;
        Object eofval = null;
        LispReaderFx instance = new LispReaderFx(tk);
        Object result = instance.read(eofError, eofval);

        System.out.println("result=" + result);

        assertTrue(result instanceof Double);
    }

    //Test
    public void testDouble2() {
        System.out.println("double-2");

        Tokenizer tk = newInstance(".3");
        boolean eofError = false;
        Object eofval = null;
        LispReaderFx instance = new LispReaderFx(tk);
        Object result = instance.read(eofError, eofval);

        System.out.println("result=" + result);

        assertTrue(result instanceof Double);
    }

    //Test
    public void testExact1() {
        System.out.println("exact-1");

        Tokenizer tk = newInstance("#e1e10");
        boolean eofError = false;
        Object eofval = null;
        LispReaderFx instance = new LispReaderFx(tk);
        Object result = instance.read(eofError, eofval);

        System.out.println("result=" + result);

        assertEquals(10000000000L, result);
    }

    //Test
    public void testExact2() {
        System.out.println("exact-2");

        Tokenizer tk = newInstance("#e3.0");
        boolean eofError = false;
        Object eofval = null;
        LispReaderFx instance = new LispReaderFx(tk);
        Object result = instance.read(eofError, eofval);

        System.out.println("result=" + result);

        assertTrue(result instanceof Ratio);
    }

    //Test
    public void testDotPair() {
        System.out.println("exact dot pair");

        Tokenizer tk = newInstance("(1 . 2)");
        boolean eofError = false;
        Object eofval = null;
        LispReaderFx instance = new LispReaderFx(tk);
        Object result = instance.read(eofError, eofval);

        System.out.println("result=" + result);

        assertTrue(result instanceof Cell);
        
        Object cdr = RT.cdr(result);
        
        assertTrue(cdr instanceof Long);
        
    }

    
    final String input1 = "(letrec ((map \n" 
            + "   (lambda (proc list)\n"
            + "      (if (pair? list)\n"
            + "         (cons (proc (car list)) (map proc (cdr list)))\n"
            + "             ()\n"
            + "         )\n"
            + "       ))\n"
            + "    (map car '((1)   (2)) )\n"
            + ")";
    
    //Test
    public void testBug1() {
        System.out.println("testBug1 EOF");

        Tokenizer tk = newInstance(input1);
        boolean eofError = false;
        Object eofval = EndOfFile.INSTANCE;
        LispReaderFx instance = new LispReaderFx(tk);
        
        RuntimeException ex = null;
        try {
        
            instance.read(eofError, eofval);

        } catch (RuntimeException re) {
            ex = re;
        }
        /*
        System.out.println("result=" + CurrentPort.printString(   result));
        
        Object res2 = RT.cdr(result);
        System.out.println("res2=" + CurrentPort.printString(   res2));

        Object res3 = RT.cdr(res2);
        System.out.println("res3=" + CurrentPort.printString(   res3));
*/
        

        assertTrue(ex instanceof ReaderException);
        
        ReaderException re2 = (ReaderException) ex;
        System.out.println("" + re2.getMessage());
        System.out.println("getLine=" + re2.getLine());
        System.out.println("getCharPos=" + re2.getCharPos());
    }
    
    final String input2 = "#;a 2 \n" ;
    

    //Test
    public void testSharpSemic1() {
        System.out.println("testSharpSemic1 ");

        Tokenizer tk = newInstance(input2);
        boolean eofError = false;
        Object eofval = EndOfFile.INSTANCE;
        LispReaderFx instance = new LispReaderFx(tk);
        
        

        Object result = instance.read(eofError, eofval);

        System.out.println("result=" + result);

        assertTrue(result instanceof Long);

    }


    final String input3 = "#!fold-case ABC" ;


    //Test
    public void testFold1() {
        System.out.println("testFold1 ");

        Tokenizer tk = newInstance(input3);
        boolean eofError = false;
        Object eofval = EndOfFile.INSTANCE;
        LispReaderFx instance = new LispReaderFx(tk);

        Object result = instance.read(eofError, eofval);

        System.out.println("result=" + result);
        assertTrue(result instanceof SSymbol);
    }
    
    final String input4 = "#!no-fold-case ABC" ;

    //Test
    public void testFold2() {
        System.out.println("testFold2 ");

        Tokenizer tk = newInstance(input4);
        boolean eofError = false;
        Object eofval = EndOfFile.INSTANCE;
        LispReaderFx instance = new LispReaderFx(tk);

        Object result = instance.read(eofError, eofval);

        System.out.println("result=" + result);
        assertTrue(result instanceof SSymbol);
    }
    
    
    //Test
    public void testEscape2() {
        System.out.println("testEscape2 ");

        ArrayList<Integer> data = new ArrayList<>();
        data.add((int)   '"');
        data.add((int)   'l');
        data.add((int)   'i');
        data.add((int)   'n');
        data.add((int)   'e');
        data.add((int)   ' ');
        data.add((int)   '1');
        data.add((int)   '\\');
        data.add((int)   ' ');
        data.add((int)   9);
        data.add((int)   ' ');
        data.add((int)   '\n');
        data.add((int)   ' ');
        data.add((int)   9);
        data.add((int)   ' ');
        
        data.add((int)   'c');
        data.add((int)   'o');
        data.add((int)   'n');
        data.add((int)   't');
        data.add((int)   'i');
        data.add((int)   'n');
        data.add((int)   'u');
        data.add((int)   'e');
        data.add((int)   'd');
        data.add((int)   '\n');
        data.add((int)   '"');
        
        SString str = SString.fromList(data);
        
        
        StringInputPort port = new StringInputPort(str);
        
        Tokenizer tk = new Tokenizer(port);
        boolean eofError = false;
        Object eofval = EndOfFile.INSTANCE;
        LispReaderFx instance = new LispReaderFx(tk);

        Object result = instance.read(eofError, eofval);

        System.out.println("result=" + result);
        assertTrue(result instanceof SString);
    }
    

    public void testUndef() {
        System.out.println("testUndef ");

        Tokenizer tk = newInstance("#<Undef>");
        boolean eofError = false;
        Object eofval = EndOfFile.INSTANCE;
        LispReaderFx instance = new LispReaderFx(tk);

        Object result = instance.read(eofError, eofval);

        System.out.println("result=" + result);
        assertTrue(result instanceof Undef);
    }

    @Test
    public void testMojiretsu() {
        System.out.println("testMojiretsu ");

        Tokenizer tk = newInstance("\"abc\"");
        boolean eofError = false;
        Object eofval = EndOfFile.INSTANCE;
        LispReaderFx instance = new LispReaderFx(tk);

        Object result = instance.read(eofError, eofval);

        System.out.println("result=" + result);
        assertTrue(result instanceof SString);
    }

}
