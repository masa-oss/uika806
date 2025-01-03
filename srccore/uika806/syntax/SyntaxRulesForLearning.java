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
import java.util.Map;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import uika806.objects.Cell;
import uika806.objects.EmptyList;
import uika806.objects.SSymbol;
import uika806.pico.macro.IMacro;
import uika806.objects.SArray;
import uika806.objects.SChar;
import uika806.objects.SString;
import uika806.objects.ScmUniqueSymbol;

import uika806.port.CurrentPort;

/**
 * 学習用に改造したもの
 * 
 * Original: com.github.chungkwong.jschememin.type.ScmSyntaxRules
 *
 */
public class SyntaxRulesForLearning implements IMacro {

    private static final Logger LOG = LoggerFactory.getLogger(SyntaxRulesForLearning.class);

    private static final SSymbol ELLIPSIS = new SSymbol("...");
    private static final SSymbol WILDCARD = new SSymbol("_");

    private final List<SyntaxRule> rules = new ArrayList<>();

    private final SSymbol ellipsis;

    private final HashSet<SSymbol> literals = new HashSet<>();
    private final Environ defEnv;

    public SyntaxRulesForLearning(Object spec, Environ env) {
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
    public SyntaxRulesForLearning(Environ defEnv) {

        ellipsis = ELLIPSIS;
        this.defEnv = defEnv;
    }

    private void addSyntaxRule(Cell rule) {

        rules.add(new SyntaxRule(SUtil.getCdar(rule), SUtil.getCadr(rule)));
    }

    public Object transform(Object argument, Environ env) {

        for (SyntaxRule rule : rules) {
            Object transformed = rule.apply(argument, env);

            if (transformed != null) {
                LOG.info("110) transformed = {}", transformed);
              //  LOG.info("110) transformed = {}", CurrentPort.printString(transformed));
                return transformed;
            }
        }
        throw new SyntaxException();
    }

    // Line 239
    private Object transform(Object temp, HashMap<SSymbol, CapturedObjects> bind, boolean ellipsed, Environ env, MultiIndex index) {
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

    // Line 244
    private Object transformSymbol(SSymbol temp, HashMap<SSymbol, CapturedObjects> bind, Environ env, MultiIndex index) {

        if (bind.containsKey(temp)) {
            return bind.get(temp).get(index);
        }
        Optional<Object> defVal = defEnv.getOptional(temp);
        SSymbol rename = new ScmUniqueSymbol(temp);

        bind.put(temp, new Rename(rename));
        if (defVal.isPresent()) {
            env.add(rename, defVal.get());
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

    static boolean isSelfevaluating(Object obj) {

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

            if (literals.contains(patt)) {
                
                if (expr instanceof ScmUniqueSymbol) {
                    expr = ((ScmUniqueSymbol) expr).getOrigin();
                }
                 

                return expr instanceof SSymbol && ((expr.equals(patt) && !defEnv.containsKey(patt) && !env.containsKey((SSymbol) expr))
                        || (defEnv.containsKey(patt) && env.containsKey((SSymbol) expr) && defEnv.get((SSymbol) patt).equals(env.get((SSymbol) expr))));
            } else if (patt.equals(WILDCARD)) {
                return true;
            } else {
                if (!bind.containsKey(patt)) {
                    bind.put(patt, new CapturedObjects());
                }
                bind.get(patt).add(index, expr);
                return true;
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

//        public Object apply(Object argument, Environ env) {
        public Map<SSymbol, CapturedObjects> apply(Object argument, Environ env) {
            
            HashMap<SSymbol, CapturedObjects> bind = new HashMap<>();
            if (match(argument, this.pattern, bind, env, new MultiIndex())) {

              //  return transform(template, bind, false, env, new MultiIndex());
                return bind;


            } else {
                return null;
            }
        }

    } // end of SyntaxRule class 

}
