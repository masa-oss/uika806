/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.vm4.vm5;

import uika806.syntax.Environ;

/**
 *
 */
public class Sresult {
    
    public final Environ environ;
    public final Object form;
    
    public Sresult(Object form, Environ environ) {
        this.form = form;
        this.environ = environ;
    }
    
}
