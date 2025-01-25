/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.pico.fn;

import org.slf4j.LoggerFactory;
import uika806.kernel.AFn;
import uika806.err.LispException;
import uika806.objects.SSymbol;
import uika806.objects.Undef;
import uika806.syntax.Environ;

/**
 *
 */
public class DefineFn extends AFn {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(DefineFn.class);
    
    @Override
    public String getName() {
        return "--define4";
    }


    @Override
    public Object invokeWithEnv(Object arg1, Object arg2, Environ env) {

        LOG.info("21)  --define4 {}, {}, {}", arg1, arg2, env);

        if (arg1 instanceof SSymbol) {
            SSymbol sym = (SSymbol) arg1;

            env.add(sym, arg2);
            
            return Undef.Undefined;
        }

        throw new LispException("Bad argument for --define4");

    }

}
