/*
 * Copyright (C) 2017 Chan Chung Kwong <1m02math@126.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * https://github.com/chungkwong/JSchemeMin
 *
 *
 *
 * Masahito Hemmi modified this source file.
 */
package uika806.syntax;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uika806.objects.Cell;
import uika806.objects.EmptyList;
import uika806.objects.SSymbol;
import uika806.objects.SArray;
import uika806.objects.SChar;
import uika806.objects.SString;
import uika806.objects.ScmUniqueSymbol;
import uika806.port.CurrentPort;

/**
 * Original: com.github.chungkwong.jschememin.type.ScmSyntaxRules
 *
 */
public class SyntaxRules /*implements IMacro*/ {
    //                    ▲ 2025-01-10 remove

    private static final Logger LOG = LoggerFactory.getLogger(SyntaxRules.class);

    //                           省略記号
    private static final SSymbol ELLIPSIS = new SSymbol("...");
    private static final SSymbol WILDCARD = new SSymbol("_");

    final List<SyntaxRule> rules = new ArrayList<>();

    private final SSymbol ellipsis;

    private final HashSet<SSymbol> literals = new HashSet<>();
    private final Environ defEnv;

    // ▼▼ add
    private String defineTo = "??"; //外側のdefine-syntax で定義した名前

    static boolean TRACE = false;
    // ▲▲ add
    
    

    public SyntaxRules(Object spec, Environ env) {
        /*
		if(spec.getCar() instanceof ScmSymbol){
			ellipsis=(ScmSymbol)spec.getCar();
			spec=(ScmPair)spec.getCdr();
		}else {
			ellipsis=ELLIPSIS;
                }
         */
        ellipsis = ELLIPSIS;

        /*
		ScmList.forEach((ScmPairOrNil)spec.getCar(),
                        (id)->  {  literals.add((ScmSymbol)id);  }  );
                
		ScmList.forEach((ScmPairOrNil)spec.getCdr(),(rule)->addSyntaxRule((ScmPair)rule));
         */
        if (spec instanceof Cell) {
            Cell cell = (Cell) spec;
            Object car = cell.getCar();
            if (car instanceof SSymbol) {
                throw new UnsupportedOperationException("syntax-rules");
            }

            while (car instanceof Cell) {
                Cell cel2 = (Cell) car;
                literals.add((SSymbol) cel2.getCar());
                car = cel2.getCdr();
            }

            Object cdr = cell.getCdr();
            while (cdr instanceof Cell) {
                Cell cel3 = (Cell) cdr;
                addSyntaxRule((Cell) cel3.getCar());
                cdr = cel3.getCdr();
            }
        }
        LOG.info("literals={}", literals);

        this.defEnv = env;
    }

    // add Hemmi
    public SyntaxRules(Environ defEnv) {

        ellipsis = ELLIPSIS;
        this.defEnv = defEnv;
    }

    private void addSyntaxRule(Cell rule) {
        
        Object pat = SUtil.getCdar(rule);
        Object temp = SUtil.getCadr(rule);
        
        if (TRACE) {
            LOG.info ("pat = {}",   CurrentPort.printLong(pat));
            LOG.info ("temp = {}",    CurrentPort.printLong(temp));
        }        
        rules.add(new SyntaxRule(pat, temp));
    }

    /*
    public String toExternalRepresentation() {
        StringBuilder buf = new StringBuilder();
        buf.append("(syntax-rules ");
        buf.append(ellipsis.getValue());
        buf.append(literals.stream().map((id) -> id.getValue()).collect(Collectors.joining(" ", " (", ") ")));
        for (SyntaxRule rule : rules) {
            buf.append('(').append(rule.pattern).append(' ').append(rule.template).append(')');
        }
        buf.append(')');
        return buf.toString();
    }
     */
    public Object transform(Object argument, Environ env) {

        LOG.info("159) *********** SyntaxRules.name ={}     start", defineTo);
        String str0 = CurrentPort.printString(argument);
        LOG.info("159) *********** before ={}", str0);
        String sEnv = env.printEnv();
        LOG.info("160) ***********      env   ={}", env.printEnv());

        int idx = 0;
        for (SyntaxRule rule : rules) {
            if (TRACE) {
                LOG.info("166) ------- {}  check rule[ {} ]", defineTo, idx);
            }
            Object transformed = rule.apply(argument, env);
            if (transformed != null) {

                String str = CurrentPort.printLong(transformed);
                LOG.info("170) *********** SyntaxRules {} ,          end,  transformed ={}", defineTo, str);

                return transformed;
            }
            idx++;
        }
        throw new SyntaxException("SyntaxName = "  + defineTo + ", " + str0 + ", env = " + sEnv);
    }

    // Line 239
    private Object transform(Object temp, HashMap<SSymbol, CapturedObjects> bind, boolean ellipsed, Environ env, MultiIndex index) {

     //   LOG.info("transform enter: {}",  CurrentPort.printLong(temp)  );

        if (temp instanceof SSymbol) {
            return transformSymbol((SSymbol) temp, bind, env, index);
        } else if (isSelfevaluating(temp)) {
            return temp;
        } else if (temp instanceof Cell) {
            if (((Cell) temp).getCar().equals(ellipsis) && !ellipsed) {
                return transform(SUtil.getCadr(temp), bind, true, env, index);
            } else {
                return transformList(temp, bind, ellipsed, env, index);
            }
        } else if (temp instanceof SArray) {
            return transformVector((SArray) temp, bind, ellipsed, env, index);
        } else {
            return temp;
        }
    }

    // Line 275
    private Object transformList(Object temp, HashMap<SSymbol, CapturedObjects> bind, boolean ellipsed, Environ env, MultiIndex index) {
        ListBuilder buf = new ListBuilder();
        while (temp instanceof Cell) {
            Object sub = ((Cell) temp).getCar();
            if (((Cell) temp).getCdr() instanceof Cell
                    && SUtil.getCadr(temp).equals(ellipsis)
                    && !ellipsed) {

                index.push();
                try {
                    while (true) {
                        buf.add(transform(sub, bind, ellipsed, env, index));
                        index.advance();
                    }
                } catch (RuntimeException ex) {
                }
                index.pop();
                temp = ((Cell) temp).getCdr();
            } else {
                buf.add(transform(sub, bind, ellipsed, env, index));
            }
            temp = ((Cell) temp).getCdr();
        }
        if (!(temp instanceof EmptyList)) {
            buf.setLast(transform(temp, bind, ellipsed, env, index));
        }
        return buf.toList();
    }

    final boolean disableLambdaConvert = false;

    // Line 244
    private Object transformSymbol(SSymbol temp, HashMap<SSymbol, CapturedObjects> bind, Environ env, MultiIndex index) {

        if (bind.containsKey(temp)) {
            // 既に bind に登録されている時
            //  return bind.get(temp).get(index);
            CapturedObjects get = bind.get(temp);
            LOG.info("229)  temp={}, get={}, index={}" , temp, get , index  );
            Object obj = get.get(index);
            LOG.info("232)  temp={}, obj={}" , temp, obj   );
            return obj;
        }
        
        // ▼ add
        if (disableLambdaConvert && SSymbol.LAMBDA.equals(temp)) {
            return temp;
        }
        // ▲ add
        
        Optional<Object> defVal = defEnv.getOptional(temp);
        SSymbol rename = new ScmUniqueSymbol(temp);

        bind.put(temp, new Rename(rename));
        if (defVal.isPresent()) {
            
            Object value = defVal.get();
            
            env.add(rename, value);
            LOG.info("251) env.add   key={}, value={}", rename, value);
        } else {
            LOG.info("253) No value found in env. temp = {}", temp);
        }
        return rename;
    }

    // Line 255            
    private Object transformVector(SArray temp, HashMap<SSymbol, CapturedObjects> bind, boolean ellipsed, Environ env, MultiIndex index) {

        ArrayList<Object> list = new ArrayList<>();
        for (int i = 0; i < temp.length(); i++) {
            if (i + 1 < temp.length() && temp.getNth(i + 1).equals(ellipsis) && !ellipsed) {
                index.push();
                try {
                    while (true) {
                        list.add(transform(temp.getNth(i), bind, ellipsed, env, index));
                        index.advance();
                    }
                } catch (RuntimeException ex) {
                }
                index.pop();
                ++i;
            } else {
                list.add(transform(temp.getNth(i), bind, ellipsed, env, index));
            }
        }
        Object[] oarr = list.toArray();
        return new SArray(oarr);
    }

    // Hemmi
    public SyntaxRule createRule(Object pattern, Object template) {
        return new SyntaxRule(pattern, template);
    }
    // ▼▼ add
    public void setName(String name) {
        defineTo = name;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("SyntaxRules[");
        sb.append("name=");
        sb.append(defineTo);

        sb.append(", defEnv=");
        sb.append(defEnv.toString());

        sb.append(", reservedWords=");
        sb.append(this.literals.toString());

        sb.append(", # of rules = ");
        sb.append(rules.size());
        sb.append("]");
        return sb.toString();
    }
    // ▲▲ add

    static boolean isSelfevaluating(Object obj) {
        
        if (obj instanceof Cell) {
            return false;
        }
        return true;
    }

    static boolean old_isSelfevaluating(Object obj) {

        if (obj instanceof Number) {
            return true;
        }

        if (obj instanceof SString) {
            return true;
        }
        if (obj instanceof EmptyList) {
            return true;
        }
        if (obj instanceof SChar) {
            return true;
        }

        return false; // 暫定
    }

    public class SyntaxRule {

        final Object pattern;
        final Object template;

        public SyntaxRule(Object pattern, Object template) {
            this.pattern = pattern;
            this.template = template;
        }

        public boolean match(Object expr, Object patt,
                HashMap<SSymbol, CapturedObjects> bind, Environ env, MultiIndex index) {

            if (patt instanceof SSymbol) {
                return matchIdentifier(expr, (SSymbol) patt, bind, env, index);
            } else if ((patt instanceof Cell)
                    || (patt instanceof EmptyList)) {
                return matchList(expr, patt, bind, env, index);
            } else if (patt instanceof SArray) {
                return matchVector(expr, (SArray) patt, bind, env, index);
            } else if (isSelfevaluating(patt)) {
                return patt.equals(expr);
            } else {
                throw new SyntaxException();
            }
        }

        private void collectPatternVariables(Object patt,
                HashMap<SSymbol, CapturedObjects> bind) {

            if (patt instanceof SSymbol) {
                bind.put((SSymbol) patt, new CapturedObjects());
            } else if (patt instanceof Cell) {
                collectPatternVariables(((Cell) patt).getCar(), bind);
                collectPatternVariables(((Cell) patt).getCdr(), bind);
            } else if (patt instanceof SArray) {
                //  ((SArray) patt).stream().forEach((p) -> collectPatternVariables(p, bind));

                SArray arr = (SArray) patt;
                int len = arr.length();
                for (int i = 0; i < len; i++) {
                    Object elem = arr.getNth(i);
                    collectPatternVariables(elem, bind);
                }

            }
        }

        private boolean matchList(Object expr, Object patt,
                HashMap<SSymbol, CapturedObjects> bind,
                Environ env, MultiIndex index) {

            while (patt instanceof Cell) {

                Object sub = ((Cell) patt).getCar();
                if (((Cell) patt).getCdr() instanceof Cell && SUtil.getCadr(patt).equals(ellipsis)) {
                    index.push();
                    int count = SUtil.getLength(expr) - SUtil.getLength(patt) + 2;
                    if (count == 0) {
                        collectPatternVariables(sub, bind);
                    }
                    while (--count >= 0 && expr instanceof Cell) {
                        if (!match(SUtil.first(expr), sub, bind, env, index)) {
                            return false;
                        }
                        expr = ((Cell) expr).getCdr();
                        index.advance();
                    }
                    index.pop();
                    patt = ((Cell) patt).getCdr();
                } else {
                    if (!(expr instanceof Cell) || !match(SUtil.first(expr), sub, bind, env, index)) {
                        return false;
                    }
                    expr = ((Cell) expr).getCdr();
                }
                patt = ((Cell) patt).getCdr();
            }
            if (!(patt instanceof EmptyList)) {
                return match(expr, patt, bind, env, index);
            } else {
                return expr instanceof EmptyList;
            }
        }

        private boolean matchIdentifier(Object expr, SSymbol patt, HashMap<SSymbol, CapturedObjects> bind,
                Environ env, MultiIndex index) {

            boolean b = matchIdentifier2(expr, patt, bind, env, index);
            if (TRACE) {
                LOG.info("398) matchIdentifier({}, {})  --> {}", expr, patt, b);
            }
            return b;
        }

        private boolean matchIdentifier2(Object expr, SSymbol patt, HashMap<SSymbol, CapturedObjects> bind,
                Environ env, MultiIndex index) {

            if (TRACE) {
                LOG.info("405) ------------ patt = {}", patt);
                LOG.info("405) ------------ literals = {}", literals);
            }
            if (literals.contains(patt)) {

                LOG.info("410) ------------ literals");

                if (expr instanceof ScmUniqueSymbol) {
                    expr = ((ScmUniqueSymbol) expr).getOrigin();
                }

                /*
                return expr instanceof SSymbol && 
                        ((expr.equals(patt) && !defEnv.containsKey(patt) && !env.containsKey((SSymbol) expr))
                        || (defEnv.containsKey(patt) && env.containsKey((SSymbol) expr) && defEnv.get((SSymbol) patt).equals(env.get((SSymbol) expr))));
                 */
                if (expr instanceof SSymbol) {
                    // exprとpatt (どちらもシンボル)が一致していて、どちらも環境に定義されていない
                    boolean bool1 = (expr.equals(patt) && !defEnv.containsKey(patt) && !env.containsKey((SSymbol) expr));

                    boolean bool2 = isMatchSym2(patt, env, expr);

                    LOG.info("429) bool1={}, bool2={}", bool1, bool2);

                    return bool1 || bool2;
                } else {
                    return false;
                }

            } else if (patt.equals(WILDCARD)) {
                if (TRACE) {
                    LOG.info("435) ------------ WILDCARD");
                }
                return true;
            } else {
                if (TRACE) {
                    LOG.info("441) ------------ ELSE");
                }
                if (!bind.containsKey(patt)) {
                    bind.put(patt, new CapturedObjects());
                }
                bind.get(patt).add(index, expr);
                return true;
            }
        }

        private boolean isMatchSym2(SSymbol patt, Environ env, Object expr) {
            
            //   boolean b3 = (defEnv.containsKey(patt) && env.containsKey((SSymbol) expr) && defEnv.get((SSymbol) patt).equals(env.get((SSymbol) expr)))
            if (TRACE) {
                LOG.info("452) expr={}, patt={}, env={}", expr, patt, env);
            }
            boolean b1 = defEnv.containsKey(patt);
            if (TRACE) {
                LOG.info("455) b1={}", b1);
            }
            if (b1) {
                boolean b2 = env.containsKey((SSymbol) expr);
                if (TRACE) {
                    LOG.info("459) b2={}", b2);
                }
                if (b2) {

                    Object get1 = defEnv.get((SSymbol) patt);
                    Object get2 = env.get((SSymbol) expr);
                    if (TRACE) {
                        LOG.info("465) get1={}, get2={}", get1, get2);
                    }
                    return get1.equals(get2);
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }

        private boolean matchVector(Object expr, SArray patt,
                HashMap<SSymbol, CapturedObjects> bind,
                Environ env, MultiIndex index) {
            return false;
            /*
            if (!(expr instanceof ScmVector)) {
                return false;
            }
            ScmVector exp = (ScmVector) expr;
            int split = patt.getLength();
            for (int i = 0; i < patt.getLength(); i++) {
                if (ellipsis.equals(patt.get(i))) {
                    split = i - 1;
                }
            }
            for (int i = 0; i < split; i++) {
                if (!match(exp.get(i), patt.get(i), bind, env, index)) {
                    return false;
                }
            }
            if (split < patt.getLength()) {
                index.push();
                collectPatternVariables(patt.get(split), bind);
                for (int i = split; i < split + exp.getLength() - patt.getLength() + 2; i++) {
                    if (!match(exp.get(i), patt.get(split), bind, env, index)) {
                        return false;
                    }
                    index.advance();
                }
                index.pop();
            }
            for (int i = split + 2; i < patt.getLength(); i++) {
                if (!match(exp.get(i + exp.getLength() - patt.getLength()), patt.get(i), bind, env, index)) {
                    return false;
                }
            }
            return true;
             */
        }

        public Object apply(Object argument, Environ env) {
            HashMap<SSymbol, CapturedObjects> bind = new HashMap<>();

            boolean b = match(argument, pattern, bind, env, new MultiIndex());
            /*        {
                String strA = CurrentPort.printString(argument);
                LOG.info("296) argument: {}", strA);
                String strP = CurrentPort.printString(pattern);
                LOG.info("296) pattern: {}", strP);
            }  */

            if (b) {
                return transform(template, bind, false, env, new MultiIndex());
            } else {
                return null;
            }
        }

    } // end of SyntaxRule class 

}
