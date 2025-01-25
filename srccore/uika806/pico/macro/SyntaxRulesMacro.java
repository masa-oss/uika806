/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.pico.macro;

import uika806.kernel.AFn;
import uika806.syntax.Environ;

import uika806.syntax.SyntaxRules;

/**
 * 
 * 
 * 
 * <code>
 * (define-syntax unless
 *    (syntax-rules ()
 *       ((unless test result1 result2 ...)
 *        (if (not test)
 *            (begin result1 result2 ...)))))
 *
 * </code>
 *
 */
public class SyntaxRulesMacro extends AFn implements IMacro {

    @Override
    public String getName() {
        return "SyntaxRulesMacro";
    }

    @Override
    public Object invokeWithEnv(Object arg1, Environ env) {

        SyntaxRules rules = new SyntaxRules(arg1, env);
        return rules;
    }
}
