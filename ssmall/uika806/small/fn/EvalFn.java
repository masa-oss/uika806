/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.small.fn;

import uika806.err.BadArgumentInFunctionException;
import uika806.kernel.AFn;
import uika806.kernel.VMLogger;
import uika806.small.env.BuiltInFuncsImpl2;
import uika806.syntax.Environ;
import uika806.vm4.Compile5;
import uika806.vm4.Op;
import uika806.vm4.VM4;

public class EvalFn extends AFn {

    @Override
    public String getName() {
        return "eval";
    }

    @Override
    public Object invoke(Object expr, Object oenv) {
        
        if (!(oenv instanceof Environ)) {
            throw new BadArgumentInFunctionException("eval");
        }


        Environ env = (Environ) oenv;
        
        VMLogger log = null;
        // VMLogger log = new VMLoggerImpl();

        Compile5 comp = new Compile5(log);

        Op op = comp.invoke(expr, Op.HALT, env);

        BuiltInFuncsImpl2 bu = new BuiltInFuncsImpl2();

        VM4 vm = new VM4(op, env, log, bu);

        return vm.exec();
    }

}
