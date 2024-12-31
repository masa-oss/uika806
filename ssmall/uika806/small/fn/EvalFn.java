package uika806.small.fn;

import uika806.kernel.AFn;
import uika806.kernel.VMLogger;
import uika806.small.env.BuiltInFuncsImpl2;
import uika806.syntax.Environ;
import uika806.vm4.Compile4;
import uika806.vm4.Op;
import uika806.vm4.VM4;

/**
 *
 * @author hemmi
 */
public class EvalFn extends AFn {

    @Override
    public String getName() {
        return "eval";
    }

    @Override
    public Object invoke(Object expr, Object oenv) {
        
        Environ env = (Environ) oenv;
        
        VMLogger log = null;
        // VMLogger log = new VMLoggerImpl();

        Compile4 comp = new Compile4(log, env);

        Op op = comp.invoke(expr, Op.HALT);

        BuiltInFuncsImpl2 bu = new BuiltInFuncsImpl2();

        VM4 vm = new VM4(op, env, log, bu);

        return vm.exec();

    }

}
