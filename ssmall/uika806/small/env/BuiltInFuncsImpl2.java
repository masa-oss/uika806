/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.small.env;

import uika806.kernel.AFn;
import uika806.kernel.BuiltInFuncs;
import uika806.syntax.Environ;

/**
 * BuiltInFuncsの実装クラス。
 * 関数（手続き）がAFnを継承して実装されている事を知っている。
 */
public class BuiltInFuncsImpl2 implements BuiltInFuncs {

    @Override
    public boolean isProcedure(Object a) {
        
        if (a instanceof AFn) {
            return true;
        }
        return false;
    }

    @Override
    public Object builtInCall(Object a, Object r, Environ env) {

        if (a instanceof AFn) {

            AFn fn = (AFn) a;
            return fn.applyTo(r, env);
        }
        throw new RuntimeException("not impl yet");
    }

}
