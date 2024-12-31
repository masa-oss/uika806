package uika806.portfn;

import uika806.kernel.AFn;
import uika806.kernel.RT;
import uika806.port.OutputPort;

/**
 *
 * @author hemmi
 */
public class WriteU8Fn extends AFn {

    @Override
    public String getName() {
        return "write-u8";
    }

    @Override
    public Object invoke(Object arg1, Object arg2) {
        
        if (!(arg1 instanceof Long)) {
            throw new IllegalArgumentException("write-u8");
        }
        if (!(arg2 instanceof OutputPort)) {
            throw new IllegalArgumentException("write-u8");
        }
        
        Long sc = (Long) arg1;
        OutputPort op = (OutputPort) arg2;
        
        long ln = sc.longValue();
        if (ln < 0 || 255 < ln) {
            throw new IllegalArgumentException("write-u8");
        }
        
        
        op.writeByte((int) ln);
        
        return RT.UNDEF;
    }
    
}
