/**
 * SPDX-License-Identifier: MPL-2.0
 * Author:  Masahito Hemmi.
 */
package uika806.vm4.vm5;

import java.util.ArrayList;
import java.util.List;
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
 *
 * 以下のJavaのコードは、startup5.scmの中に定義されている --letrec に、どのようなコードが書かれているかを知っている。
 * そのコードを知っている前提で、環境の処理をしているので、startup5.scmの中の定義を書き換えると以下のJavaは正しく動かなくなる。
 *
 * </code>
 */
public class LetrecsyntaxHelper {
    
    private static final Logger LOG = LoggerFactory.getLogger(LetrecsyntaxHelper.class);
    
    static SSymbol SEARCH = new SSymbol("--letrec");
    
    public Sresult doSpecial(Cell seq, Environ environ) {
        
        Optional<Object> optional = environ.getOptional(SEARCH);
        
        if (!optional.isPresent()) {
            throw new LispException("'--letrec' not found in env.");
        }
        
        Object get = optional.get();
        
        LOG.info("45) {}", get);
        
        if (!(get instanceof SyntaxRules)) {
            throw new LispException("'--letrec' was not SyntaxRules.");
        }
        
        SyntaxRules sr = (SyntaxRules) get;
        
        LOG.info("53) {}", CurrentPort.printLong(seq));
        
        Object transformed = sr.transform(seq.getCdr(), environ);
        
        LOG.info("57) transformed={}", CurrentPort.printLong(transformed));
        
        while (transformed instanceof Cell) {
            Cell cTrans = (Cell) transformed;
            
            if (isHletrec(cTrans)) {
                transformed = sr.transform(cTrans.getCdr(), environ);
                LOG.info("56) transformed={}", CurrentPort.printLong(transformed));
            } else {
                break;
            }
        }
        
        LOG.info("70) transformed={}", CurrentPort.printLong(transformed));

        //  ここで必ず、以下のようなletになっている筈
/*
transformed=
        (let#71 ((my-or #<Undef>))
           (let#71 ((newtemp#70 
        
        (syntax-rules ()
          ((my-or) #f)
          ((my-or e) e)
          ((my-or e1 e2 ...)
           (let ((temp e1)) (if temp temp (my-or e2 ...)))))))
        (set!#72 my-or newtemp#70)
        (let ((x #f)
               (y 7)
                (temp 8)
                (let odd?)
                (if even?))
          (my-or x (let temp) (if y) y))))

        
         */
        Sresult outer = procOuter(transformed, environ);
        LOG.info("99) form={}", CurrentPort.printLong(outer.form));
        LOG.info("100) outer env={}", outer.environ.printEnv());

        Sresult inner = procInner(outer.form, outer.environ);
        LOG.info("101) inner env={}", inner.environ.printEnv());

        Sresult result = procSet(inner.form, inner.environ);
        LOG.info("103) form={}", CurrentPort.printLong(result.form));
        LOG.info("104) env={}", result.environ.printEnv());
        return result;
    }
    
    Sresult procOuter(Object seq, Environ environ) {
        
        Object car = RT.car(seq);
        checkLet(car);
        Object[] split = splitBind(RT.cadr(seq));
        List<Object> var1 = (List<Object>) split[0];
        Object obj = arrayToCell(var1);
        Object undef = sameLengthUndef(obj);
        
        Environ extend = environ.extend(obj, undef);
        LOG.info("113) env-step1 = {}", extend.printEnv());
        Object rest = RT.cddr(seq);
        rest = RT.car(rest);   // 式は１つと仮定
        LOG.info("114) rest = {}", CurrentPort.printLong(rest));
        
        return new Sresult(rest, extend);
    }
    
    Sresult procInner(Object seq, Environ environ) {
        
        Object car = RT.car(seq);
        checkLet(car);
        Object[] split = splitBind(RT.cadr(seq));
        List<Object> var1 = (List<Object>) split[0];
        Object obj = arrayToCell(var1);
        
        List<Object> val1 = (List<Object>) split[1];
        Object syntax = arrayToSyntax(val1, environ);   //*****

        Environ extend = environ.extend(obj, syntax);
        LOG.info("133) env-step2 = {}", extend.printEnv());
        Object rest = RT.cddr(seq);
        LOG.info("135) rest = {}", CurrentPort.printLong(rest));
        
        return new Sresult(rest, extend);
    }
    
    Sresult procSet(Object body, Environ environ) {
        
        while (body instanceof Cell) {
            Object car = RT.car(body);
            if (car instanceof Cell) {
                Cell cell = (Cell) car;
                
                if (isSet(cell.getCar())) {
                    Object cdr = cell.getCdr();
                    Object var1 = RT.car(cdr);
                    Object val1 = RT.cadr(cdr);  // 変数になっている筈
                    LOG.info("156) var = {}, val = {}", CurrentPort.printLong(var1), CurrentPort.printLong(val1));
                    
                    Optional<Object> value = environ.getOptional((SSymbol) val1);
                    
                    if (! value.isPresent()) {
                        throw new LispException("letrec-syntax(161)" +  CurrentPort.printLong(val1)  );
                    }
                    
                    environ.set((SSymbol) var1, value.get());
                    LOG.info("161) env = {}", environ.printEnv());
                    
                    body = RT.cdr(body);
                    continue;
                    
                } else {
                    
                    Object result = new Cell(new SSymbol("begin"), body);
                    LOG.info("158) result = {}", CurrentPort.printLong(result));
                    
                    return new Sresult(result, environ);
                }
                
            } else {
                throw new LispException("letrec-syntax(159)");
            }
        }
        throw new LispException("letrec-syntax(163)");
    }
    
    public static Object arrayToSyntax(List<Object> list, Environ env) {
        
        Object var = RT.EOL;
        int len = list.size();
        for (int i = len - 1; i >= 0; i--) {
            Object o = list.get(i);
            LOG.info("149) o = {}", CurrentPort.printLong(o));
            Object cdr = RT.cdr(o);
            SyntaxRules sr = new SyntaxRules(cdr, env);
            LOG.info("152) SyntaxRules = {}", sr.toString());
            var = new Cell(sr, var);
        }
        return var;
    }
    
    boolean isSet(Object form) {
        
        if (form instanceof ScmUniqueSymbol) {
            ScmUniqueSymbol sus = (ScmUniqueSymbol) form;
            SSymbol origin = sus.getOrigin();
            if ("set!".equals(origin.getName())) {
                return true;
            }
            return false;
        } else {
            return false;
        }
    }
    
    void checkLet(Object form) {
        
        if (form instanceof ScmUniqueSymbol) {
            ScmUniqueSymbol sus = (ScmUniqueSymbol) form;
            SSymbol origin = sus.getOrigin();
            if ("let".equals(origin.getName())) {
                return;
            }
            throw new LispException("letrec-syntax (115)");
        } else {
            throw new LispException("letrec-syntax (117)");
        }
    }
    
    public static Object arrayToCell(List<Object> list) {
        
        Object var = RT.EOL;
        int len = list.size();
        for (int i = len - 1; i >= 0; i--) {
            var = new Cell(list.get(i), var);
        }
        return var;
    }
    
    public static Object[] splitBind(Object x) {
        
        List<Object> var = new ArrayList<>();
        List<Object> val = new ArrayList<>();
        
        Object binds = x;
        while (!(RT.isNull(binds))) {
            
            Object car = RT.car(binds);
            var.add(RT.car(car));
            val.add(RT.cadr(car));
            binds = RT.cdr(binds);
        }
        
        return new Object[]{var, val};
    }
    
    public static Object sameLengthUndef(Object list) {
        
        if (!(list instanceof Cell)) {
            return RT.EOL;
        }
        return new Cell(Undef.Undefined, sameLengthUndef(RT.cdr(list)));
    }
    
    boolean isHletrec(Cell list) {
        
        Object car = list.getCar();
        if (car instanceof ScmUniqueSymbol) {
            ScmUniqueSymbol sus = (ScmUniqueSymbol) car;
            String nm = sus.getOrigin().getName();
            boolean b = "-letrec".equals(nm);
            return b;
        }
        return false;
    }
    
}
