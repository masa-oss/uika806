/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.pico.fn;

import java.util.Map;
import org.slf4j.LoggerFactory;

import uika806.lib.LibraryManager;
import uika806.lib.Library;
import uika806.kernel.AFn;
import uika806.objects.Cell;
import uika806.kernel.RT;
import uika806.objects.SSymbol;
import uika806.syntax.Environ;

/**
 *
 */
public class SimportFn extends AFn {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(SimportFn.class);
    
    @Override
    public String getName() {
        return "*import";
    }


    @Override
    public Object invokeWithEnv(Object arg1, Environ env) {
        
        if (env == null) throw new NullPointerException();

        if (arg1 instanceof Cell) {
            Object name = arg1;

            while (name instanceof Cell) {

                Object car = RT.car(name);
                Library lib = LibraryManager.findLib((Cell) car);

                LOG.info("31)   lib = {}", lib);

                
                if (lib != null) {

                    Map<SSymbol, Object> map = lib.getUnmodifiableMap();
                    map.entrySet().iterator().forEachRemaining((e) -> env.define(e.getKey(), e.getValue()));

                  //  throw new RuntimeException();
                }

                name = RT.cdr(name);
            }

            return Boolean.TRUE;
        }
        throw new IllegalArgumentException("import : " + arg1);
    }

}
