/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.vm4.vm5;

import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uika806.err.LispException;
import uika806.objects.Cell;
import uika806.objects.SSymbol;
import uika806.objects.ScmUniqueSymbol;
import uika806.port.CurrentPort;
import uika806.syntax.Environ;
import uika806.syntax.SyntaxRules;

/**
 * <code>
 * define-syntaxで、bodyを展開し、環境は、Javaで変更する
 *
 * 
 * 以下のJavaのコードは、startup5.scmの中に定義されている --letrec に、どのようなコードが書かれているかを知っている。
 * そのコードを知っている前提で、環境の処理をしているので、startup5.scmの中の定義を書き換えると以下のJavaは正しく動かなくなる。
 * 
 * </code>
 */
public class HletrecHelper {

    private static final Logger LOG = LoggerFactory.getLogger(HletrecHelper.class);

    static SSymbol SEARCH = new SSymbol("--letrec");

    public Sresult doSpecial(Cell seq, Environ environ) {

        Optional<Object> optional = environ.getOptional(SEARCH);

        if (!optional.isPresent()) {
            throw new LispException("'--letrec' not found in env.");
        }

        Object get = optional.get();

        LOG.info("36) {}", get);

        if (!(get instanceof SyntaxRules)) {
            throw new LispException("'--letrec' was not SyntaxRules.");
        }

        SyntaxRules sr = (SyntaxRules) get;

        LOG.info("44) {}", CurrentPort.printLong(seq));

        Object transformed = sr.transform(seq.getCdr(), environ);

        LOG.info("48) transformed={}", CurrentPort.printLong(transformed));
        
        while (transformed instanceof Cell) {
            Cell cTrans = (Cell) transformed;
            
            if (isHletrec(cTrans) ) {
                transformed = sr.transform(cTrans.getCdr(), environ);
                LOG.info("56) transformed={}", CurrentPort.printLong(transformed));

            } else {
                break;
            }
        }
        
        
        LOG.info("70) transformed={}", CurrentPort.printLong(transformed));
        
        //  ここで必ず、以下のようなletになっている筈
        //   (let#5 ((a #<Undef>) (b #<Undef>)) (let#5 ((newtemp#4 1) (newtemp#2 2)) (set!#6 a newtemp#4) (set!#6 b newtemp#2) (list a b)))
        //  そのため、環境を拡張する必要はない。(letが環境を拡張するから)

        
        return new Sresult(transformed, environ);
    }

    boolean isHletrec(Cell list) {
        
        Object car = list.getCar();
        if (car instanceof ScmUniqueSymbol) {
            ScmUniqueSymbol sus = (ScmUniqueSymbol)car;
            String nm = sus.getOrigin().getName();
            boolean b = "-letrec".equals(nm);
            return b;
        }
        return false;
    }
    
}
