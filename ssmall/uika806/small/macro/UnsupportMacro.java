package uika806.small.macro;

import uika806.err.LispException;
import uika806.pico.macro.IMacro;
import uika806.kernel.AFn;

/**
 * 
 * 
 */
public class UnsupportMacro extends AFn implements IMacro {

    String name;
    
    public UnsupportMacro(String name) {
        this.name = name;
    }
    @Override
    public String getName() {
        return name;
    }

    @Override
    public Object invoke(Object arg1) {
        throw new LispException("Unsupport macro " + name);
    }
}
