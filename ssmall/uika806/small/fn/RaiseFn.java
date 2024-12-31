package uika806.small.fn;

import org.slf4j.LoggerFactory;

import uika806.kernel.AFn;
import uika806.err.RaiseException;
import uika806.kernel.RT;
import uika806.port.CurrentPort;

/**
 *
 * @author hemmi
 */
public class RaiseFn extends AFn {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(RaiseFn.class);
    
    @Override
    public String getName() {
        return "raise";
    }

    @Override
    public Object invoke(Object arg1) {
        
        String sArg1 = CurrentPort.printString(arg1);
        LOG.info("arg1={}", arg1);
        
        throw new RaiseException("raise", RT.list(arg1), false);
    }
    
    @Override
    public Object invoke(Object arg1, Object arg2) {
        
        throw new RaiseException("raise", RT.list(arg1, arg2), false);
    }
}
