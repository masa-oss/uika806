/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.small.fn;

import org.slf4j.LoggerFactory;
import uika806.Services;
import uika806.kernel.AFn;
import uika806.kernel.RT;
import uika806.objects.Cell;
import uika806.port.CurrentPort;

/**
 *
 */
public class EnvironmentFn extends AFn {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(EnvironmentFn.class);

    @Override
    public String getName() {
        return "environment";
    }

    @Override
    public Object invoke(Object arg1) {
        return getEnv(RT.list(arg1));
    }

    @Override
    public Object invoke(Object arg1, Object arg2) {
        return getEnv(RT.list(arg1, arg2));
    }

    @Override
    public Object invoke(Object arg1, Object arg2, Object arg3) {
        return getEnv(RT.list(arg1, arg2, arg3));
    }

    Object getEnv(Object list1) {
        
        
        String str = CurrentPort.printString(list1);
        LOG.info("47) {}", str);
        
        
        Cell list = (Cell) list1;
        Object o = Services.environFactory.getEnviron(list);
        return o;
    }
}
