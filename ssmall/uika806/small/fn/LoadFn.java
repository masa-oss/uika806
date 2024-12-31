package uika806.small.fn;


import java.io.IOException;
import org.slf4j.LoggerFactory;


import uika806.small.env.Loader;
import uika806.err.LispException;
import uika806.kernel.AFn;
import uika806.kernel.RT;
import uika806.objects.SString;
import uika806.syntax.Environ;

/**
 *
 * @author hemmi
 */
public class LoadFn extends AFn {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(LoadFn.class);
    
    Loader loader = new Loader();
    
    @Override
    public String getName() {
        return "load";
    }

    
    
    @Override
    public Object invokeWithEnv(Object arg1, Environ env) {
        
        if (arg1 instanceof SString) {
            SString ss = (SString) arg1;
            
            try {
                loader.load(ss.toString(), env);
            } catch (IOException ioe) {
                throw new LispException("IOException", ioe);
            }
            
            return RT.UNDEF;
        }
        throw new LispException("Bad arg for load");
    }

    
}
