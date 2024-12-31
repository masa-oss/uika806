package uika806.pico.fn;

import org.slf4j.LoggerFactory;
import uika806.kernel.AFn;
import uika806.objects.SSymbol;
import uika806.syntax.Environ;

/**
 *
 * Move uika806.small.fn to xxx.pico.fn
 */
public class SDefMacroFn extends AFn {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(SDefMacroFn.class);
    
    @Override
    public String getName() {
        return "*defmacro";
    }

    
    
    @Override
    public Object invokeWithEnv(Object arg1, Object arg2, Environ env) {

        LOG.info("arg1={}", arg1);
//        LOG.info("arg2={}", arg2);
//        LOG.info("env={}", env);
        env.define((SSymbol) arg1   , arg2);
        
        return 123L;
    }
    
}
