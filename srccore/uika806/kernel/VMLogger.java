/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.kernel;

import uika806.syntax.Environ;

/**
 *
 */
public interface VMLogger {

    void writeLog(Object a, Object x, Environ e, Object r, Object s);

    String printString(Object obj);
}
