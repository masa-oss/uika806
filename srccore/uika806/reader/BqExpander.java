/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.reader;

import org.slf4j.LoggerFactory;
import uika806.err.ReaderException;
import uika806.kernel.RT;
import uika806.objects.Cell;
import uika806.objects.EmptyList;
import uika806.objects.SSymbol;

/**
 *
 */
public class BqExpander {
    
    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(BqExpander.class);
    
    
    public Object expand_bq(Object s) {

        if (!(s instanceof Cell)) {
            return RT.list(SSymbol.QUOTE, s);
        }

        if (SSymbol.UNQUOTE.equals(RT.car(s))) {
            return RT.cadr(s);
        }

        if (SSymbol.UNQUOTE_SPLICE.equals(RT.car(s))) {
            throw new ReaderException(",@ error");
        }

//        if (Flistp(Fcar(s)) != Qnil && Fcar(Fcar(s))== Qbqatmark)
//		return list3(Qappend, Fcdr(Fcar(s)), expand_bq(Fcdr(s)));
        if (RT.isList(RT.car(s)) != false && SSymbol.UNQUOTE_SPLICE.equals(RT.caar(s))) {
            return RT.list(SSymbol.APPEND, RT.cadar(s), expand_bq(RT.cdr(s)));
        }

//        if (Flistp(Fcar(s)) != Qnil && Fcar(Fcar(s)) == Qbqdot)
//		return list3(Qnconc, Fcdr(Fcar(s)), expand_bq(Fcdr(s)));
        Object a = expand_bq(RT.car(s));
        Object d = expand_bq(RT.cdr(s));

        /*        
	if (NILP(Fcdr(s)))
		return list2(Qlist, a);
	if (Flistp(d) != Qnil && Fcar(d) == Qlist)
		return list_star3(Qlist, a, Fcdr( d));
	return list3(Qcons, a, d);
         */        
        if (RT.cdr(s) instanceof EmptyList) {
            return RT.list(SSymbol.LIST, a);
        } else {
            return RT.list(SSymbol.CONS, a, d);
        }
        
    }
    
}
