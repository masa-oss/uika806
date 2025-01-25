package uika806.syntax;

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
public class SyntaxRules2Test {

    public SyntaxRules2Test() {
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
        Object result = instance.read(eofError, eofval);
        return result;
    }

    static String COND1
            = "(syntax-rules (else =>)"
            + "    ((cond (else result1 result2 ...))"
            + "     (begin result1 result2 ...))"
            + ")";

    static String COND2
            = "(syntax-rules (else =>)"
            + " ((cond (test => result))"
            + "    (let ((temp test))"
            + "      (if temp (result temp))))   )";

    PrinterSchemeLimit printer = new PrinterSchemeLimit();

    @Test
    public void testMatch1() {

        System.out.println("testMatch1");

        SchemeEnvironment env = new SchemeEnvironment(true);
        env.add(SSymbol.SYN_ELSE, SSymbol.SYN_ELSE);
        
        env.add(SSymbol.SYN_GT, SSymbol.SYN_GT);  // これを追加する (=>)
        

        Object form = read(COND2);
        System.out.println("form=" + form);

        Object cdr = RT.cdr(form);

        SyntaxRules instance = new SyntaxRules(cdr, env);

        var argument = read("(cond  (#t => 'ok ) )");
        argument = RT.cdr(argument);

        // 新しい環境をletする
        
        env.add(SSymbol.SYN_GT, 0L);  // (let (=> 0)
        
        Object result = instance.transform(argument, env);

        System.out.println("result=" + printer.prin1(result));
    }

}
