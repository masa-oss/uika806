package uika806.pico.fn;

import uika806.err.LispException;
import uika806.port.CurrentPort;
import uika806.kernel.AFn;
import static uika806.kernel.RT.UNDEF;
import uika806.port.OutputPort;

/**
 *
 * @author hemmi
 */
public class NewLineFn extends AFn {

    @Override
    public String getName() {
        return "newline";
    }

    @Override
    public Object invoke() {
        
        return invoke(CurrentPort.OUTPUT_PORT);
        
    }

    @Override
    public Object invoke(Object arg1) {

        if (!(arg1 instanceof OutputPort)) {
            throw new LispException("Bad arg for newline");
        }
        
        
        OutputPort op = (OutputPort) arg1;
        
        op.write("\n");
        op.flush();
        return UNDEF;
    }
    
}
