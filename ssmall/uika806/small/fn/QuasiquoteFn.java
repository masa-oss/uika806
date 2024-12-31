/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.small.fn;

import uika806.kernel.AFn;
import uika806.reader.BqExpander;

/**
 *
 * @author hemmi
 */
public class QuasiquoteFn extends AFn {
    
    final BqExpander bqExpander = new BqExpander();

    @Override
    public String getName() {
        return "quasiquote";
    }

    @Override
    public Object invoke(Object arg1) {
        return bqExpander.expand_bq(arg1);
    }
    
}
