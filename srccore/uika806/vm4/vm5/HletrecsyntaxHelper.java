package uika806.vm4.vm5;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uika806.err.LispException;
import uika806.kernel.RT;
import uika806.objects.Cell;
import uika806.objects.SSymbol;
import uika806.objects.ScmUniqueSymbol;
import uika806.objects.Undef;
import static uika806.pico.macro.LetRecMacro.sameLengthUndef;
import static uika806.pico.macro.LetRecMacro.splitBind2;
import uika806.port.CurrentPort;
import uika806.syntax.Environ;
import uika806.syntax.SyntaxRules;

/**
 * <code>
 * 
 * </code>
 */
public class HletrecsyntaxHelper {

    private static final Logger LOG = LoggerFactory.getLogger(HletrecsyntaxHelper.class);


    public static Object[] splitBind(Object x) {

        List<Object> var = new ArrayList<>();
        List<Object> val = new ArrayList<>();

        Object binds = x;
        while (!(RT.isNull(binds))) {

            Object car = RT.car(binds);
            var.add(RT.car(car));
            val.add(RT.cadr(car));
            binds = RT.cdr(binds);
        }

        return new Object[]{var, val};
    }
    
    public static Object arrayToCell(List<Object> list) {

        Object var = RT.EOL;
        int len = list.size();
        for (int i = len - 1; i >= 0; i--) {
            var = new Cell(list.get(i), var);
        }
        return var;
    }
    
    
    public Sresult doSpecial(Cell _seq, Environ environ) {
        
        Object seq = _seq.getCdr();

        Object binds = RT.car(seq);
        Object body = RT.cdr(seq);

        // (letrec-syntax ((a (syntax-rules ...)))  body)
        LOG.info("64) binds = {}", CurrentPort.printLong(binds));
        LOG.info("65) body = {}", CurrentPort.printLong(body));
        
        Object[] pair = splitBind(binds);
        
        List<Object> var = (List<Object>) pair[0];
        List<Object> val = (List<Object>) pair[1];
        Object var2 = arrayToCell(var);
        Object undefs = sameLengthUndef(var2);
        
        Environ env1 = environ.extend(var2, undefs);
        LOG.info("75) env1 = {}", env1.printEnv());
        
        ArrayList<SyntaxRules> sList = new ArrayList<>();
        int nLen = val.size();
        for (int idx = 0; idx < nLen; idx++) {
            Object rules = val.get(idx);
            LOG.info("81) body={}", CurrentPort.printLong(rules));
            SyntaxRules sr = new SyntaxRules( RT.cdr( rules), env1);
            SSymbol sym = (SSymbol) var.get(idx);
            sr.setName(  sym.getName()  );
            LOG.info("85) SyntaxRules = {}", sr.toString());
            sList.add(sr);
        }
        
        for (int j = 0; j < nLen; j++) {
            SSymbol sym = (SSymbol) var.get(j);
            SyntaxRules sr1 = sList.get(j);
            env1.set(sym, sr1);
        }

        LOG.info("94) env1 = {}", env1.printEnv());
        
            
        return new Sresult(body, env1);
    }

    
    public static Object sameLengthUndef(Object list) {

        if (!(list instanceof Cell)) {
            return RT.EOL;
        }
        return new Cell(Undef.Undefined, sameLengthUndef(RT.cdr(list)));
    }
}
