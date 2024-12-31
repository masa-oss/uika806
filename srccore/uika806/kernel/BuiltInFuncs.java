/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.kernel;

import uika806.syntax.Environ;

/**
 * 組み込み関数（手続き）が、どのように実装されているかを、VMからは見えなくするために存在する。
 * 
 * 現在の実装は、AFnを継承して、関数を作っているが、これを介する事で、VMからは実装の詳細が見えなくなる。
 */
public interface BuiltInFuncs {

    boolean isProcedure(Object a);

    Object builtInCall(Object a, Object r, Environ env);

}
