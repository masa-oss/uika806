package uika806.syntax;

import java.util.List;
import java.util.Map;
import org.junit.Test;
import uika806.fn011.reader.Tokenizer;
import uika806.kernel.RT;
import uika806.objects.SSymbol;
import uika806.port.CodepointLispStream;
import uika806.print.PrinterSchemeLimit;
import uika806.reader.LispReaderEx;

/**
 *
 * @author hemmi
 */
public class SyntaxRulesForLarningTest {
    
    public SyntaxRulesForLarningTest() {
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
    
    static String COND1 = "(syntax-rules (else =>)"
            + " ((cond (else result1 result2 ...))"
            + " (begin result1 result2 ...))"
            + ")"
            ;
    
    
    
    public void testTransform() {
        
        System.out.println("transform");
        
        SchemeEnvironment env = new SchemeEnvironment(true);
        env.add(SSymbol.SYN_ELSE, SSymbol.SYN_ELSE);
        
        
        Object form = read(COND1);
        System.out.println("form="+form);
        
        Object cdr = RT.cdr(form);
        
        SyntaxRulesForLearning instance = new SyntaxRulesForLearning(cdr, env);


        Object argument = read("(cond (else 1 2))");

        Object expResult = null;
        Object result = instance.transform(argument, env);


        System.out.println("result=" + result);
    }

    PrinterSchemeLimit printer = new PrinterSchemeLimit();
    
    static String BEGIN = "(syntax-rules ()"
            + "  ((begin exp ...)"
            + "   ((lambda () exp ...))))";
    

    @Test
    public void testTransform2() {
        
        System.out.println("transform2");
        
        SchemeEnvironment env = new SchemeEnvironment(true);
        env.add(SSymbol.SYN_ELSE, SSymbol.SYN_ELSE);
        
        
        Object form = read(COND1);
        System.out.println("form="+ printer.prin1(  form));
        
        Object cdr = RT.cdr(form);
        
        SyntaxRulesForLearning instance = new SyntaxRulesForLearning(cdr, env);


        Object argument = read("(begin a b)");
        argument = read("((else a b c d ))");

        Object result = instance.transform(argument, env);


        System.out.println("result=" + (    result));

        
        Map<SSymbol, CapturedObjects> map = (Map<SSymbol, CapturedObjects>) result;


        for (SSymbol sym : map.keySet()) {
            
            System.out.println("sym=" + sym);
            CapturedObjects cap = map.get(sym);
            
            dumpCapturedObjects(cap);
        }

    }


    void dumpCapturedObjects(CapturedObjects x) {
        
        List<Object> unmodifiableList = x.getUnmodifiableList();
        for (Object o : unmodifiableList) {
            System.out.println("o = " + o);
            
        }
        
    }

    
    
    
}
