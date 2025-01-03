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
import java.util.stream.Collectors;
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
 * Original: com.github.chungkwong.jschememin.type.ScmSyntaxRules
 *
 */
public class SyntaxRulesMine implements IMacro {

    private static final Logger LOG = LoggerFactory.getLogger(SyntaxRulesMine.class);

    private static final SSymbol ELLIPSIS = new SSymbol("...");
 //   private static final SSymbol WILDCARD = new SSymbol("_");

  //  private final List<SyntaxRule> rules = new ArrayList<>();

    private final SSymbol ellipsis;

 //   private final HashSet<SSymbol> literals = new HashSet<>();
    private final Environ defEnv;
/*
*/
    // Constuctor by  Hemmi
    public SyntaxRulesMine(Environ defEnv) {

        ellipsis = ELLIPSIS;
        this.defEnv = defEnv;
    }

    // テスト対象メソッド(publicに変更)
    // Line 239
    public Object transform(Object temp, HashMap<SSymbol, CapturedObjects> bind, boolean ellipsed, Environ env, MultiIndex index) {

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

    // Line 275(publicに変更)
    public Object transformList(Object temp, HashMap<SSymbol, CapturedObjects> bind, boolean ellipsed, Environ env, MultiIndex index) {

        ListBuilder buf = new ListBuilder();
        while (temp instanceof Cell) {
            Object sub = ((Cell) temp).getCar();
            if (((Cell) temp).getCdr() instanceof Cell
                    && SUtil.getCadr(temp).equals(ellipsis)  // ...
                    && !ellipsed) {

                index.push();
                try {
                    while (true) {
                        Object o102 = transform(sub, bind, ellipsed, env, index);
                        buf.add(o102);
                        index.advance();
                    }
                } catch (RuntimeException ex) {
                    LOG.error("RuntimeException", ex);
                }
                index.pop();
                temp = ((Cell) temp).getCdr();
            } else {
                Object o110 = transform(sub, bind, ellipsed, env, index);
                buf.add(o110);
            }
            temp = ((Cell) temp).getCdr();
        }
        if (!(temp instanceof EmptyList)) {
            Object o116 = transform(temp, bind, ellipsed, env, index);
            buf.setLast(o116);
        }
        return buf.toList();
    }

    // Line 244(publicに変更)
    public Object transformSymbol(SSymbol temp, HashMap<SSymbol, CapturedObjects> bind, Environ env, MultiIndex index) {

        if (bind.containsKey(temp)) {
            
          //  return bind.get(temp).get(index);
            CapturedObjects get = bind.get(temp);
            Object get1 = get.get(index);
            return get1;
        }
        Optional<Object> defVal = defEnv.getOptional(temp); // SyntaxRuleを定義した時の環境に定義されている？
        SSymbol rename = new ScmUniqueSymbol(temp);

        bind.put(temp, new Rename(rename));
        if (defVal.isPresent()) {             // 定義されていたら
            env.add(rename, defVal.get());    // 現在の環境に定義する
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
/*
    public class SyntaxRule {


    } // end of SyntaxRule class 
*/
}
