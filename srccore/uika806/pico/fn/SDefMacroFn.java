/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.pico.fn;

import org.slf4j.LoggerFactory;
import uika806.kernel.AFn;
import uika806.objects.SSymbol;
import uika806.objects.Undef;
import uika806.syntax.Environ;
import uika806.syntax.SyntaxRules;

/**
 *
 */
public class SDefMacroFn extends AFn {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(SDefMacroFn.class);
    
    @Override
    public String getName() {
        
//        return "*defmacro";
        
        return "SDefMacroFn";
    }

    
    
    @Override
    public Object invokeWithEnv(Object arg1, Object arg2, Environ env) {

        LOG.info("arg1={}", arg1);
        LOG.info("arg2={}", arg2);
        LOG.info("env={}", env);
        
        if (arg2 instanceof SyntaxRules) {
            SyntaxRules sr = (SyntaxRules) arg2;
            sr.setName(arg1.toString());
        }
        
        
        env.define((SSymbol) arg1   , arg2);
        
        return Undef.Undefined;
    }
    
}
