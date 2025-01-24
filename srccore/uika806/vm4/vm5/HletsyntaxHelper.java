package uika806.vm4.vm5;

import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uika806.err.LispException;
import uika806.kernel.RT;
import uika806.objects.Cell;
import uika806.objects.SSymbol;
import uika806.objects.ScmUniqueSymbol;
import uika806.objects.Undef;
import uika806.port.CurrentPort;
import uika806.syntax.Environ;
import uika806.syntax.SyntaxRules;

/**
 * <code>
 * define-syntaxで、bodyを展開し、環境は、Javaで変更する
 *
 * 以下のJavaのコードは、startup5.scmの中に定義されている --let に、どのようなコードが書かれているかを知っている。
 * そのコードを知っている前提で、環境の処理をしているので、startup5.scmの中の定義を書き換えると以下のJavaは正しく動かなくなる。
 * 
 * </code>
 */
public class HletsyntaxHelper {

    private static final Logger LOG = LoggerFactory.getLogger(HletsyntaxHelper.class);

    static SSymbol SEARCH = new SSymbol("--let");

    public Sresult doSpecial(Cell seq, Environ environ) {

        Optional<Object> optional = environ.getOptional(SEARCH);

        if (!optional.isPresent()) {
            throw new LispException("'--let' not found in env.");
        }

        Object get = optional.get();

        LOG.info("36) {}", get);

        if (!(get instanceof SyntaxRules)) {
            throw new LispException("'--let' was not SyntaxRules.");
        }

        SyntaxRules sr = (SyntaxRules) get;
        
        // (let ((=> #f)) (cond (#t => (quote ok))))
        LOG.info("44) {}", CurrentPort.printLong(seq));

        Object transformed = sr.transform(seq.getCdr(), environ);

        LOG.info("48) {}", CurrentPort.printLong(transformed));
        // ((lambda#52 (=>) (cond (#t => (quote ok)))) #f)
        
        Object lambdaList = isLambda(transformed);
        
        if (lambdaList != null) {
            LOG.info("59) {}", CurrentPort.printLong(lambdaList));
            
            Object undefList = mustbeSyntaxRules(RT.cdr(transformed), environ);
            
            Environ newE = environ.extend(lambdaList, undefList);  // 拡張した環境を作る
            
            return new Sresult(transformed, newE);
        } else {
            return new Sresult(transformed, environ);
        }
    }

    public static Object mustbeSyntaxRules(Object list, Environ env) {

        if (!(list instanceof Cell)) {
            return RT.EOL;
        }
        Cell cell = (Cell) list;
        Object car = cell.getCar();
        LOG.info("79) {}", CurrentPort.printLong(car));
        Cell cell2 = (Cell) car;
        
        SyntaxRules rules = new SyntaxRules(cell2.getCdr(), env);
        
        LOG.info("81) {}", rules.toString());

        return new Cell(rules, mustbeSyntaxRules(RT.cdr(list), env));
    }


    /**
     * <code>
     * もし以下のような入力の場合
     * ((lambda (a b) (list a b)) 1 2)
     * は、引数である (a b)を返す。
     * 
     * それ以外の場合は、nullを返す。
     * </code>
     * @param form
     * @return 
     */
    Object isLambda(Object form) {
        
        if (form instanceof Cell) {
            Cell cell = (Cell) form;
            Object car = cell.getCar();
            if (car instanceof Cell) {
                Object caar = ((Cell) car).getCar();
                Object cdar = ((Cell) car).getCdr();
                
                if (caar instanceof ScmUniqueSymbol) {
                    ScmUniqueSymbol sus = (ScmUniqueSymbol) caar;
                    SSymbol sym = sus.getOrigin();
                    
                    LOG.info("106) sym= {}", sym);
                    if ("lambda".equals(sym.getName())) {
                        
                        if (cdar instanceof Cell) {
                            Cell cc = (Cell) cdar;
                            return cc.getCar();
                        }
                    }
                    return null;
                }
                
                
                if (caar instanceof SSymbol) {
                    SSymbol sym = (SSymbol) caar;
                    
                    LOG.info("120) sym= {}", sym);
                    if ("lambda".equals(sym.getName())) {
                        
                        if (cdar instanceof Cell) {
                            Cell cc = (Cell) cdar;
                            return cc.getCar();
                        }
                    }
                }
            }
        }
        return null;
    }
    
    


}
