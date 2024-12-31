package uika806.pico.fn;

import uika806.kernel.AFn;
import uika806.err.LispException;
import uika806.port.InputPort;

/**
 *
 * Move uika806.small.fn to xxx.pico.fn
 */
public class CloseInputPortFn extends AFn {

    @Override
    public String getName() {
        return "close-input-port";
    }

    @Override
    public Object invoke(Object arg1) {

        if (arg1 instanceof InputPort) {
            InputPort cop = (InputPort) arg1;
            cop.close();
            return Boolean.TRUE;
        } else {
            throw LispException.illegalArgument("Bad argument for close-input-port", arg1);
        }

    }
    
}
