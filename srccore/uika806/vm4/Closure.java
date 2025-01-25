/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.vm4;

import uika806.port.OutputPort;
import java.io.IOException;
import uika806.err.FileException;
import uika806.kernel.PrintOption;
import uika806.kernel.SelfPrintable;

/**
 *
 */
public final class Closure implements SelfPrintable {

    private final Object body;
    private final Object env;
    private final Object vars;

    public Closure(Object compiled, Object e, Object vars) {
        this.body = compiled;
        this.env = e;
        this.vars = vars;
    }

    /**
     * @return the body
     */
    public Object getBody() {
        return body;
    }

    /**
     * @return the env
     */
    public Object getEnv() {
        return env;
    }

    /**
     * @return the vars
     */
    public Object getVars() {
        return vars;
    }

    @Override
    public String toString() {
    //    return "<procedure>";
        return "<closure>";
    }

    @Override
    public void prin1(OutputPort outport, PrintOption opt) {

        String print = "<procedure ??>";

        try {
            int len = print.length();
            for (int i = 0; i < len; i++) {

                char ch = print.charAt(i);
                outport.write((int) ch);
            }
        } catch (IOException ioe) {
            throw new FileException("IOException", ioe);
        }
    }

}
