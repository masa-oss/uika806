package uika806.pico.macro;

import java.util.ArrayList;
import org.junit.Test;
import static org.junit.Assert.*;

import uika806.fn011.reader.Tokenizer;
import uika806.objects.EndOfFile;
import uika806.pico.fn.EqualFn;
import uika806.port.CodepointLispStream;
import uika806.port.CurrentPort;
import uika806.reader.LispReaderEx;

/**
 *
 * @author hemmi
 */
public class LambdaMacroTest {

    public LambdaMacroTest() {
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

    public Object parseString(String str) {

        Tokenizer tk = newInstance(str);
        boolean eofError = false;
        LispReaderEx instance = new LispReaderEx(tk);
        return instance.read(eofError, EndOfFile.INSTANCE);
    }

    @Test
    public void testDefineValue() {
        System.out.println("Test (define a 123)");
        ArrayList<Object> list = new ArrayList<>();
        list.add( parseString("(define a (+ 2 3))")   );
        LambdaMacro instance = new LambdaMacro();
        
        
        LambdaMacro.ExpandResult result = instance.arrayToCell(list);

        String sResult = CurrentPort.printString(result.var);
        
        System.out.println("52) " + sResult);
        Object expResult = parseString("((a (+ 2 3)))");
        Boolean b = (Boolean) equalFn.invoke( expResult,  result.var   );

        System.out.println("55)" + b);
        assertTrue(b);
    }

    @Test
    public void testDefineFunc() {
        System.out.println("Test (define (foo x) 123)");
        ArrayList<Object> list = new ArrayList<>();
        list.add( parseString("(define (foo x) (+ 2 x))")   );
        LambdaMacro instance = new LambdaMacro();
        LambdaMacro.ExpandResult result = instance.arrayToCell(list);

        String sResult = CurrentPort.printString(result.var);
        
        System.out.println("68) " + sResult);
        
        Object expResult = parseString("((foo (-λ (x) (+ 2 x))))");
        
        Boolean b = (Boolean) equalFn.invoke( expResult,  result.var   );
        
        System.out.println("73)" + b);
        
        assertTrue(b);
    }

    EqualFn equalFn = new EqualFn();

    @Test
    public void testDefineSyntax() {
        System.out.println("TEST (define-syntax swap! (syntax-rules ...))");
        ArrayList<Object> list = new ArrayList<>();
        list.add( parseString("(define-syntax swap! (syntax-rules ))")   );
        LambdaMacro instance = new LambdaMacro();
        LambdaMacro.ExpandResult result = instance.arrayToCell(list);

        String sResult = CurrentPort.printString(result.var);
        
        System.out.println("94) " + sResult);
        
        Object expResult = parseString("((swap! (syntax-rules)))");
        
        Boolean b = (Boolean) equalFn.invoke( expResult,  result.var   );
        
        System.out.println("100)" + b);
        
        assertTrue(b);
    }

    
    
    @Test
    public void testDefineValues() {
        System.out.println("TEST (define-values (x y)  ...)");
        ArrayList<Object> list = new ArrayList<>();
        list.add( parseString(     "(define-values (x y) (integer-sqrt 17))"  )   );
        LambdaMacro instance = new LambdaMacro();
        LambdaMacro.ExpandResult result = instance.arrayToCell(list);

        String sResult = CurrentPort.printString(result.var);
        
        System.out.println("94) " + sResult);
    }

    
}
