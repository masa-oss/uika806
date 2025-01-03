package uika806.syntax;

import java.util.HashMap;
import org.junit.Test;
import static org.junit.Assert.*;
import uika806.fn011.reader.Tokenizer;
import uika806.objects.SSymbol;
import uika806.port.CodepointLispStream;
import uika806.print.PrinterSchemeLimit;
import uika806.reader.LispReaderEx;

/**
 * transformの理解の為に、書いたテスト
 *
 *
 * 2025-01-02
 */
public class SyntaxRulesMineTest {

    public SyntaxRulesMineTest() {
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

    Object read(String str) {
        
        Tokenizer tk = newInstance(str);
        boolean eofError = false;
        Object eofval = null;
        LispReaderEx instance = new LispReaderEx(tk);
        Object result = instance.read( eofError, eofval);
        return result;
    }
    
    

    @Test
    public void testTransform() {
        System.out.println("transform");
        
        Object temp = read("(a b a b)");

        SchemeEnvironment env = new SchemeEnvironment(true);

        SyntaxRulesMine instance = new SyntaxRulesMine(env);

//        Object expResult = null;

        HashMap<SSymbol, CapturedObjects> bind = new HashMap<>();
        boolean ellipsed = false;
        MultiIndex index = new MultiIndex();
        Object result = instance.transform(temp, bind, ellipsed, env, index);
        
        
        System.out.println("41) " + printer.prin1(result)  );
        
        System.out.println("71) " + bind);
        
//        assertEquals(expResult, result);
    }

    
    PrinterSchemeLimit printer = new PrinterSchemeLimit();
    
    
    
    //Test
    public void testTransformList() {
        System.out.println("transformList");
        Object temp = null;
        HashMap<SSymbol, CapturedObjects> bind = null;
        boolean ellipsed = false;
        Environ env = null;
        MultiIndex index = null;
        SyntaxRulesMine instance = null;
        Object expResult = null;
        Object result = instance.transformList(temp, bind, ellipsed, env, index);
//        assertEquals(expResult, result);
    }

    //Test
    public void testTransformSymbol() {
        System.out.println("transformSymbol");
        SSymbol temp = null;
        HashMap<SSymbol, CapturedObjects> bind = null;
        Environ env = null;
        MultiIndex index = null;
        SyntaxRulesMine instance = null;
        Object expResult = null;
        Object result = instance.transformSymbol(temp, bind, env, index);
//        assertEquals(expResult, result);
    }

}
