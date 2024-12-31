package uika806.small.fn;

import org.slf4j.LoggerFactory;
import uika806.err.BadArgumentInFunctionException;
import uika806.err.LispException;
import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.objects.SSymbol;
import uika806.port.CurrentPort;
import uika806.syntax.Environ;
import uika806.syntax.SyntaxRules;

/**
 *
 * @author hemmi
 */
public class MacroExpandFn extends AFn {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(MacroExpandFn.class);
    
    
    @Override
    public Object invokeWithEnv(Object arg1, Environ env) {
        
        if (arg1 instanceof Cell) {
            
            
            LOG.info("arg1 = {}", CurrentPort.printString(arg1));
            Cell cell = (Cell) arg1;
            Object car = cell.getCar();
            if (car instanceof SSymbol) {
                SSymbol sym = (SSymbol ) car;
                
                Object get = env.get(sym);
                if (get == null) {
                    throw new LispException("Not found " + sym.toString());
                }
                if (get instanceof SyntaxRules) {
                    SyntaxRules sr = (SyntaxRules) get;
                    
                    LOG.info("invoke SyntaxRules : {}" , sr.toString());
                    
                    return sr.transform(cell.getCdr(), env);
                 //   return sr.transform(cell, env);
                    
                }
                if (get instanceof AFn) {
                    AFn afn = (AFn) get;
                    
                    return afn.invoke(cell.getCdr());
                }
                
                
                throw new LispException("Is not a SyntaxRules : " + sym.toString());
            }
            throw new BadArgumentInFunctionException("macro-expand");
        }
        throw new BadArgumentInFunctionException("macro-expand");
    }


    @Override
    public String getName() {
        return "macro-expand";
    }
    
}
